# Recursive Descent: Functional Refactoring Plan

## Status (2026-08-28)

**All phases below (1 through 7, including the Phase 5 and Phase 7
stretch goals) are implemented, tested, and deployed in
`rdescent-ws.lisp`.** This document is now kept as historical design
rationale -- "why the code is shaped this way" -- rather than a
tracker of outstanding work. A functional-programming adherence audit
performed on 2026-08-28 confirmed:

- `entity`/`tile`/`game-map`/`game-state` are all `:reader`-only CLOS
  classes (no `:accessor`/`:writer` anywhere) updated exclusively via
  the `update-entity`/`update-game-state` copy-with-overrides
  functions -- there is no `setf`-able slot for any of these value
  types to even accidentally mutate in place.
- `move-player`, `process-enemy-turns`, `apply-rdescent-command`,
  `parse-rdescent-command`, `render-grid`, `rdescent-outbound-packets`,
  `add-rdescent-client`, and `remove-rdescent-client` are all pure
  (state/command/list in, fresh state/command/list out; no I/O, no
  mutation of arguments).
- The only `setf` calls against a live object in the whole file are
  the documented, single-writer publications of a fresh value:
  `(setf (rdescent-client-game-state client) ...)` (Phase 2's
  documented invariant) and `(setf (slot-value client
  'membership-tier) ...)` (written exactly once, in
  `client-connected`, before the client is ever registered or
  visible to any other thread).
- The client registry (`*rdescent-clients*`) is now owned by a single
  dedicated actor thread (`rdescent-clients-registry-loop`,
  Phase 5's stretch goal), eliminating the last lock in the file --
  every other thread only ever sends immutable messages via
  `*rdescent-clients-mailbox*`.
- The game loop (`start-game-loop`) is organized as an explicit event
  source (`rdescent-tick-events`, a `series` generator, Phase 7's
  stretch goal) -> pure transform (`rdescent-outbound-packets`) -> I/O
  sink (`hunchensocket:send-text-message`) pipeline.
- **Doc-drift fix (2026-08-28):** `parse-rdescent-command`'s docstring
  and this file's header comment previously described its fallible-
  parse chaining as using a bespoke `maybe-let*`/`maybe-bind` helper
  (per Phase 6 below); no such helper was ever actually defined --
  the code has always used `alexandria:when-let*` (Alexandria already
  being a project dependency/`:use`d package, per `package.lisp`/
  `jrm-code-project.asd`) directly. Corrected both docstrings to name
  `when-let*` accurately rather than a helper that doesn't exist, so
  a future reader grepping for `maybe-bind` doesn't waste time looking
  for a function that was never written. No behavior change.

No further phases are planned; any new game features (inventory,
combat, multi-tier persistence, etc.) should follow the same
functional-core/imperative-shell discipline established here rather
than reopening any of these phases.

---

## Goal

`rdescent-ws.lisp` currently mixes pure rendering logic with mutable
struct state, global locks, and direct I/O (WebSocket sends, thread
spawning) in the same functions. The goal of this refactor is to make
the **main interaction path** -- "client sends an action" -> "state
advances" -> "clients observe the new state" -- a functional,
stateless pipeline, and to push all side effects (mutation, sockets,
threads, locks, logging) to a thin imperative shell at the edges.

This document is a step-by-step plan, not a one-shot rewrite. Each
phase should be implemented, tested (`asdf:test-system
:jrm-code-project`), verified live, committed, and deployed
independently, per the project's established workflow, before moving
to the next phase. Phases are ordered so that each one leaves the
system fully working -- no phase depends on a later phase to compile
or pass tests.

---

## Current State (baseline)

- `game-state` is a mutable `defstruct` with `setf`-able slots.
- `move-player` mutates a `game-state` in place, guarded by a global
  `*game-state-lock*`.
- `render-grid` reads a `game-state`'s mutable slots (also under the
  lock) and produces a string -- this part is already pure once the
  values are extracted.
- `text-message-received` is a single method that parses JSON, decides
  whether the packet is a move command, mutates state, and swallows
  errors, all in one place.
- `broadcast-state` reads the global client list, renders each client's
  state, and performs the socket I/O to send it, all in one function.
- `*rdescent-clients*` is a mutable global list guarded by
  `*rdescent-clients-lock*`.
- Two `bordeaux-threads` locks and one background thread are the only
  concurrency primitives.

None of this is *wrong* -- it works, it's tested, it's deployed -- but
state, decision logic, and I/O are entangled, which makes the core game
rules hard to unit test in isolation and hard to extend (e.g. adding
new action types, monsters, inventory) without touching locking code
every time.

---

## Phase 1: Make `game-state` an immutable value type -- IMPLEMENTED

**Objective:** replace the mutable `defstruct game-state` with an
immutable value (still a `defstruct`, but with no `setf`-able slots
used outside of construction) that is always replaced, never mutated
in place, by move operations.

**Steps:**
1. Redefine `game-state` as `(defstruct (game-state (:constructor
   make-game-state (&key (player-x 40) (player-y 15)))) player-x
   player-y)` -- keep it simple; no reader-only enforcement is required
   at the language level, just a house rule (enforced by review/tests)
   that nothing outside of `make-game-state` ever calls `(setf
   (game-state-player-x ...))`.
2. Rewrite `move-player` as a **pure function** `(move-player state
   direction) -> new-game-state`, returning a fresh `game-state` via
   `make-game-state` (or `copy-game-state` + `setf` on the copy, which
   is fine since the copy is local and never shared) instead of
   mutating its argument. No lock needed inside this function anymore
   -- it has no shared state to protect.
3. `render-grid` continues to take a `game-state` value and returns a
   string; since the value is now immutable, no lock is needed to read
   it safely.

**Effect on concurrency:** once `game-state` values are immutable, the
*values* need no locking at all. The only remaining concurrency
concern becomes "how do we safely publish a new `game-state` value to
the slot on `rdescent-client` so the game loop thread sees an
up-to-date value" -- which is a much narrower problem (see Phase 2).

**Testing:** update `move-player-direction-and-bounds-pure` to assert
on the *returned* value rather than in-place mutation of the passed-in
struct; add a test asserting the original `game-state` passed to
`move-player` is unchanged after the call (proving immutability).

---

## Phase 2: Replace the per-client lock with atomic state publication -- IMPLEMENTED

**Objective:** eliminate `*game-state-lock*` by using an atomic
compare-and-swap (or a single-writer `setf` on a struct slot, which is
already atomic for a fixnum/simple-vector/single Lisp object reference
on SBCL) to publish new `game-state` values.

**Steps:**
1. Store the client's current `game-state` in an accessor backed by an
   `sb-ext:atomic` place, or simply rely on the fact that `(setf
   (rdescent-client-game-state client) new-state)` is a single
   pointer-sized write and thus already atomic on SBCL for this use
   case (single-writer-per-client: only that client's own read-loop
   thread ever calls `move-player`/writes this slot; the game-loop
   thread only reads it). Document this invariant clearly in a
   docstring since it's what lets us safely delete the lock.
2. Delete `*game-state-lock*` and all `bordeaux-threads:with-lock-held`
   forms wrapping `game-state` reads/writes.
3. Keep `*rdescent-clients-lock*` (protects the *list* of clients,
   which is genuinely multi-writer: connect/disconnect from many
   threads plus iteration from the game loop) -- this one is a true
   shared-mutable-list problem and functional immutability doesn't
   remove the need for *some* synchronization there; Phase 5 addresses
   it differently (persistent data structure) if desired.

**Testing:** no behavior change expected; full regression run only.

---

## Phase 3: Separate "decide" from "do" in the message handler -- IMPLEMENTED

**Objective:** turn `text-message-received` from "parse + validate +
mutate + log all at once" into a pure decision function plus a thin
imperative dispatcher.

**Steps:**
1. Introduce a pure function `parse-rdescent-command (raw-json-string)
   -> command-or-nil`, where a "command" is a simple tagged value, e.g.
   `(list :move "up")`, or better, a small immutable struct
   `rdescent-command` with a `kind` and `payload`. This function calls
   `cl-json:decode-json-from-string` (an I/O-adjacent but still
   referentially-transparent operation -- parsing a string is pure
   given the string) inside a `handler-case`, and returns `nil` on any
   parse failure or unrecognized shape -- **it never signals**, so
   callers don't need their own `handler-case` for control flow.
2. Introduce a pure function `apply-rdescent-command (state command) ->
   new-state`, which pattern-matches on `command`'s kind:
   - `:move` -> calls the (already pure, from Phase 1) `move-player`.
   - unrecognized/`nil` command -> returns `state` unchanged (identity).
   This is the natural "reducer" of the system: `(state, command) ->
   state`, the same shape as a Redux/Elm-style reducer, and is trivial
   to property-test (e.g. "applying an unrecognized command is always
   the identity function").
3. Rewrite the `text-message-received` method itself down to:
   ```lisp
   (defmethod hunchensocket:text-message-received ((resource rdescent-resource)
                                                     (client rdescent-client)
                                                     message)
     (let ((command (parse-rdescent-command message)))
       (when command
         (setf (rdescent-client-game-state client)
               (apply-rdescent-command (rdescent-client-game-state client) command)))))
   ```
   All the "this must never crash the read loop" concern now lives
   entirely inside `parse-rdescent-command`'s `handler-case`, and the
   logging of malformed input becomes a single, optional `warn`/log
   call at that one boundary -- not scattered logic mixed with the
   business rule.

**Testing:** `parse-rdescent-command` and `apply-rdescent-command` are
now directly unit-testable without any `rdescent-client`/`hunchensocket`
scaffolding at all (no more `make-instance 'rdescent-client
'hunchensocket::input-stream ...` boilerplate needed for command-logic
tests) -- only the thin dispatcher method needs an integration-style
test with a real client instance. This is the single biggest testing
ergonomics win in this refactor.

---

## Phase 4: Make rendering and packet construction a pure pipeline -- IMPLEMENTED

**Objective:** express "state in, JSON-string-to-send out" as a
composition of small pure functions, with a clear seam between "pure
core" and "I/O shell".

**Steps:**
1. Keep `render-grid :: game-state -> string` pure (already is, after
   Phase 1/2 drop the lock).
2. Keep `rdescent-grid-packet :: game-state -> string` (JSON-encodes
   the rendered grid) pure -- it already is, structurally; no change
   needed beyond removing the lock dependency.
3. Introduce a pure function `rdescent-outbound-packets :: game-state ->
   list-of-json-strings` if/when more than one packet must be derived
   from a state change (e.g. a move might eventually need to update
   both `playing-field` and `message-log`, such as "You hit a wall").
   For now this can be a thin wrapper returning `(list
   (rdescent-grid-packet state))`, but it establishes the seam so that
   later game features (combat log messages, inventory panels) plug in
   as additional pure entries in the list without touching I/O code.
4. `broadcast-state` becomes purely the I/O shell: snapshot the client
   list (still needs `*rdescent-clients-lock*`, Phase 2), then for each
   client, compute `(rdescent-outbound-packets (rdescent-client-game-state
   client))` (pure) and `hunchensocket:send-text-message` each one (I/O,
   wrapped in the existing per-client `handler-case`).

**Testing:** `rdescent-outbound-packets` gets a direct pure unit test;
`broadcast-state` keeps its existing (implicit, via full-stack) test
coverage since it's now "obviously correct" I/O glue around already-
tested pure functions.

---

## Phase 5: Model the client registry as a persistent (functional) collection -- IMPLEMENTED (including the step-3 stretch goal)

**Objective:** replace the mutable `push`/`remove`-on-a-global-list
pattern for `*rdescent-clients*` with an explicit, functional
"reducer" over connect/disconnect events, isolating the one place
where imperative list mutation still occurs.

**Steps:**
1. Introduce pure functions `add-rdescent-client (clients client) ->
   new-clients` (`(cons client clients)`, dedup-guarded) and
   `remove-rdescent-client (clients client) -> new-clients` (`(remove
   client clients)`), both pure list-in/list-out.
2. `client-connected`/`client-disconnected` become the imperative shell:
   read `*rdescent-clients*`, call the pure function, write back under
   `*rdescent-clients-lock*` (this is a classic "read-modify-write"
   critical section, but the *logic* being applied is now a pure,
   independently-testable function rather than inline `push`/`remove`
   forms).
3. *(Optional, if further purity is desired)* Replace the raw
   `bordeaux-threads:with-lock-held` + special variable with a
   dedicated single-threaded "registry actor": a `bordeaux-threads`
   thread owning `*rdescent-clients*` privately, communicating via a
   `sb-concurrency:mailbox` (or a simple queue) that receives
   `(:connect client)` / `(:disconnect client)` / `(:snapshot
   reply-channel)` messages. This is the "move mutation to the edges"
   idea taken to its natural conclusion: exactly one thread ever
   touches the mutable list, and every other thread only ever sends
   immutable messages to it. This step is marked optional/stretch
   because it adds real operational complexity (another thread,
   message queue, and reply-channel bookkeeping) for a codebase this
   size; do it only if the team wants to fully eliminate ad hoc locks
   project-wide, otherwise Phase 5 steps 1-2 already achieve "the
   mutation logic is pure, only the read-modify-write is imperative."

**Testing:** `add-rdescent-client`/`remove-rdescent-client` get direct
pure unit tests (including "removing a client not in the list is a
no-op", "adding the same client twice does/doesn't dedupe" per whatever
policy is chosen).

---

## Phase 6: Introduce a `result`/`maybe`-style type for fallible parsing (lightweight monadic style) -- IMPLEMENTED

**Status:** done, but via `alexandria:when-let*` (already a project
dependency/`:use`d package) rather than a hand-written `maybe-bind`
helper -- step 2 below turned out to be unnecessary once we noticed
Alexandria already ships the exact "bind" primitive this phase wanted:
`when-let*`'s bindings short-circuit to the overall body not running
(effectively returning `NIL`) the instant any earlier binding is
`NIL`, which is precisely the "maybe-bind" chaining step 2 proposed
writing by hand. `parse-rdescent-command` threads raw string ->
decoded alist -> validated `"action"="move"` -> validated `direction`
string -> `rdescent-command` struct through one `when-let*` form
exactly as step 3 describes. The convention from step 1 (pure
functions never signal; only `parse-rdescent-command`'s JSON-decode
call is wrapped in `handler-case`) is documented in this file's header
comment and `parse-rdescent-command`'s own docstring.

**Objective:** rather than reaching for a general monad library
(unnecessary dependency weight for this codebase), adopt a small,
explicit `maybe`/`result` convention already idiomatic in Lisp: "return
`nil`, or a genuinely meaningful value; use `(values value
success-p)` when `nil` itself is a valid success value." Use this
convention consistently across the new pure functions from Phases 3-5
so error handling composes predictably without exceptions crossing the
pure/impure boundary except at the one explicit
`parse-rdescent-command` seam (which is where exceptions legitimately
originate, from the JSON parser).

**Steps:**
1. Document the convention once, at the top of `rdescent-ws.lisp`:
   "Pure functions in this file signal no errors; a failed parse or an
   inapplicable command returns `NIL`/identity rather than a condition.
   The only place a Lisp condition is caught is at the JSON-parsing
   I/O boundary in `PARSE-RDESCENT-COMMAND`."
2. ~~Optionally define a tiny helper macro/function `maybe-bind`~~ --
   not needed: `alexandria:when-let*` already provides this chaining
   directly, with no new code to write or maintain.
3. Apply `when-let*` inside `parse-rdescent-command` to thread: raw
   string -> decoded alist -> validated `"action"="move"` -> validated
   `direction` string -> `rdescent-command` struct.

**Testing:** covered by `parse-rdescent-command`'s existing pure unit
tests (one assertion per validation step: malformed JSON, wrong
`action`, missing/non-string `direction`, and the success path).

---

## Phase 7 (stretch): Reactive broadcast via an explicit event stream -- IMPLEMENTED

**Objective:** the user's plan mentions "reactive programming"; the
natural, low-risk way to introduce this idea here -- without pulling in
a full FRP library -- is to replace "the game loop thread directly
calls `broadcast-state` on a timer" with an explicit **event source**
(the tick timer) feeding a **pure transform** (state -> packets) whose
output is **subscribed to** by the I/O sink (the WebSocket send). This
is reactive-programming-shaped without new dependencies:

**Steps:**
1. Define `rdescent-tick-events () -> a lazy/generator sequence of tick
   events`, e.g. using the codebase's existing `SERIES` library (already
   a project dependency per `package.lisp`) to express "every
   `*rdescent-tick-seconds*`, produce a tick" as a series, rather than
   a bare `(loop (broadcast-state) (sleep ...))`. `SERIES` is already
   the codebase's chosen tool for "compiler-optimized lazy sequence
   fusion", so reusing it here (rather than introducing e.g. a
   `cl-cont`/reactive-streams dependency) keeps this idiomatic to the
   rest of the project.
2. Define a pure per-tick transform `(client, tick) -> outbound
   packets` (this is exactly `rdescent-outbound-packets` composed with
   the per-client state lookup from Phase 4).
3. The imperative shell (`start-game-loop`) becomes "for each tick
   event, for each connected client, compute outbound packets (pure),
   then send (I/O)" -- structurally identical to today's
   `broadcast-state`, but now explicitly organized as "event source ->
   pure transform -> I/O sink" rather than one flat imperative loop
   body, which is the essence of the reactive-programming ask without
   a new dependency or a rewrite of the threading model.

This phase is explicitly a stretch goal: it changes the *shape* of the
code for stylistic/architectural clarity but does not fix a bug or add
a capability, so it should only be pursued after Phases 1-6 are merged
and stable, and only if the team agrees the `SERIES`-based expression
is actually clearer than the current five-line `loop`.

---

## Summary Table

| Phase | Removes | Adds | Risk | Status |
|---|---|---|---|---|
| 1 | Mutable `game-state` slots | Pure `move-player` returning new state | Low | Done |
| 2 | `*game-state-lock*` | Documented single-writer invariant | Low | Done |
| 3 | Mutation mixed into message handler | `parse-rdescent-command` / `apply-rdescent-command` reducer pair | Low | Done |
| 4 | Lock-coupled rendering | `rdescent-outbound-packets` pure pipeline seam | Low | Done |
| 5 | Inline `push`/`remove` on client list | `add-rdescent-client` / `remove-rdescent-client` pure reducers; registry-actor thread + `sb-concurrency` mailbox replacing the lock (stretch) | Low-Medium | Done (incl. stretch step) |
| 6 | Ad hoc error handling | Explicit `maybe`/`result` convention via `alexandria:when-let*` (no bespoke helper needed) | Low | Done |
| 7 (stretch) | Flat imperative game loop | `SERIES`-based tick event source + pure transform + I/O sink | Medium (stylistic only) | Done |

## Non-Goals

- No new external dependencies (no monad libraries, no FRP
  frameworks). The existing `SERIES`/`alexandria`/`bordeaux-threads`
  stack is sufficient.
- No behavior change visible to clients at any phase -- every phase
  must pass the full FiveAM suite unchanged in observable behavior, be
  verified live locally and in production, and be committed/deployed
  independently per the project's standard workflow.
- No premature actor-model rewrite of the client registry (Phase 5
  step 3) unless the team explicitly wants it; the read-modify-write
  lock pattern with a pure reducer function inside is sufficient
  "push side effects to the edges" for this codebase's current size.

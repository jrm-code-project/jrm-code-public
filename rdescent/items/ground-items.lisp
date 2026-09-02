;;; -*- Lisp -*-

;;; GROUND-ITEM (the ENTITY wrapping any RDESCENT-ITEM PAYLOAD lying on
;;; the dungeon floor) and every MAKE-GROUND-* factory/DEFINE-GROUND-
;;; ARMORY-ITEM/DEFINE-GROUND-CONSUMABLE-ITEM macro invocation that
;;; wraps an item from RDESCENT/ITEMS/BASE.LISP/EQUIPMENT.LISP/
;;; LOOT.LISP/CONSUMABLES.LISP in one -- plus the ground-only loot that
;;; never joins the RDESCENT-ITEM hierarchy at all: keys (MAKE-GROUND-
;;; KEY, FUTURE_PLANS.md §9) and Stock Options/Severance Packages
;;; (MAKE-GROUND-STOCK-OPTION/MAKE-GROUND-SEVERANCE-PACKAGE/MAYBE-
;;; DROP-MONSTER-RSU, this game's RSU/gold-pickup currency). Last of
;;; RDESCENT/ITEMS/*.LISP to load, since its DEFINE-GROUND-*-ITEM macro
;;; invocations all reference MAKE-* factories from the other item
;;; files.

(in-package "JRM-CODE-PROJECT")

(defclass ground-item (entity)
  ((payload :initarg :payload :reader get-payload))
  (:default-initargs :blocks-movement nil :render-order 0 :is-alive nil)
  (:documentation "An ENTITY representing loot lying on the dungeon
floor, waiting to be picked up (see GRAB-ITEM) or freshly dropped (see
DROP-ITEM). BLOCKS-MOVEMENT defaults to NIL and RENDER-ORDER to 0
(drawn underneath the player/monsters, mirroring the corpse/stairs
convention -- see MOVE-PLAYER/MAKE-STAIRS-UP) so a ground item never
obstructs movement and never hides a living actor standing on its
tile; IS-ALIVE defaults to NIL (like MAKE-STAIRS-UP/MAKE-STAIRS-DOWN's
fixtures) so PROCESS-ENEMY-TURNS never offers it a monster's turn.
PAYLOAD (read via GET-PAYLOAD) is either the keyword :KOMBUCHA
(GRAB-ITEM increments the player's own KOMBUCHA counter rather than
adding anything to INVENTORY), a cons (:STOCK-OPTION . AMOUNT)
(GRAB-ITEM increments the player's own RSU counter by AMOUNT, likewise
never touching INVENTORY -- see MAKE-GROUND-STOCK-OPTION), or an
RDESCENT-ITEM instance (GRAB-ITEM appends it to the player's INVENTORY
list, subject to RDESCENT-TIER-INVENTORY-LIMIT). See
MAKE-GROUND-KOMBUCHA/MAKE-GROUND-PIP/MAKE-GROUND-REPLY-ALL/
MAKE-GROUND-STOCK-OPTION for the concrete factories, and
SPAWN-ITEMS-FOR-LEVEL for procedural placement. A GROUND-ITEM's
PAYLOAD survives a pass through UPDATE-ENTITY (e.g. ACCRUE-ENERGY's
per-tick sweep over every entity in REDUCE-TICK) -- see UPDATE-ENTITY's
own docstring -- so a GROUND-ITEM already several ticks old is still
safely readable by GRAB-ITEM/DROP-ITEM."))

(defun make-ground-kombucha (x y level)
  "Pure factory: return a fresh GROUND-ITEM at (X, Y) on LEVEL
representing a Kombucha lying on the floor -- char #\\!, NAME
\"Kombucha\", MESSAGE-COLOR \"green\" (the color GRAB-ITEM's pickup
message is displayed in), PAYLOAD the keyword :KOMBUCHA."
  (make-instance 'ground-item :x x :y y :char #\! :name "Kombucha" :level level
                              :message-color "green" :payload :kombucha))

(defun make-ground-pip (x y level)
  "Pure factory: return a fresh GROUND-ITEM at (X, Y) on LEVEL
representing a Scroll of PIP lying on the floor -- char #\\?, NAME
\"Scroll of PIP\", MESSAGE-COLOR \"yellow\", PAYLOAD a fresh
MAKE-SCROLL-OF-PIP instance (see USE-ITEM/APPLY-ITEM for what using
one does once GRAB-ITEM has moved it into the player's INVENTORY)."
  (make-instance 'ground-item :x x :y y :char #\? :name "Scroll of PIP" :level level
                              :message-color "yellow" :payload (make-scroll-of-pip)))

(defun make-ground-reply-all (x y level)
  "Pure factory: return a fresh GROUND-ITEM at (X, Y) on LEVEL
representing a Reply-All Bomb lying on the floor -- char #\\?, NAME
\"Reply-All Bomb\", MESSAGE-COLOR \"orange\", PAYLOAD a fresh
MAKE-REPLY-ALL-BOMB instance (see USE-ITEM/APPLY-ITEM for what using
one does once GRAB-ITEM has moved it into the player's INVENTORY)."
  (make-instance 'ground-item :x x :y y :char #\? :name "Reply-All Bomb" :level level
                              :message-color "orange" :payload (make-reply-all-bomb)))

(defun make-ground-reorg-memo (x y level)
  "Pure factory: return a fresh GROUND-ITEM at (X, Y) on LEVEL
representing a Vague Re-Org Memo lying on the floor -- char #\\?, NAME
\"Vague Re-Org Memo\", MESSAGE-COLOR \"#95a5a6\" (a drab corporate
gray), PAYLOAD a fresh MAKE-REORG-MEMO instance (see USE-ITEM/
APPLY-ITEM/CAST-REORG-MEMO for what using one does once GRAB-ITEM has
moved it into the player's INVENTORY)."
  (make-instance 'ground-item :x x :y y :char #\? :name "Vague Re-Org Memo" :level level
                              :message-color "#95a5a6" :payload (make-reorg-memo)))

(defun make-ground-equippable-item (x y level char name message-color payload)
  "Pure helper shared by §13's many equippable ground-loot wrappers:
return a GROUND-ITEM at (X, Y) on LEVEL with CHAR/NAME/MESSAGE-COLOR
and RDESCENT-ITEM PAYLOAD exactly as supplied."
  (make-instance 'ground-item :x x :y y :char char :name name :level level
                              :message-color message-color :payload payload))

(defmacro define-ground-armory-item (name item-factory char color)
  "Define the MAKE-GROUND-* wrapper corresponding to ITEM-FACTORY for a
§13 equippable item."
  (let* ((item-name (symbol-name item-factory))
         (prefix-length (length "MAKE-"))
         (suffix (subseq item-name prefix-length))
         (ground-name (intern (format nil "MAKE-GROUND-~A" suffix) (symbol-package item-factory))))
    `(defun ,ground-name (x y level)
       ,(format nil "Pure factory: return a fresh GROUND-ITEM wrapping ~A." name)
       (make-ground-equippable-item x y level ,char ,name ,color (,item-factory)))))

(define-ground-armory-item "A Stack of Unread Memos (Hardbound)" make-stack-of-unread-memos #\) "#d08770")
(define-ground-armory-item "Keyboard of Kinesis (Epic)" make-keyboard-of-kinesis #\) "#d08770")
(define-ground-armory-item "Red Swingline Stapler" make-red-swingline-stapler #\) "#d08770")
(define-ground-armory-item "3-Foot Ethernet Cable (Cat 6)" make-three-foot-ethernet-cable #\) "#d08770")
(define-ground-armory-item "Severed Server Rack Rail" make-severed-server-rack-rail #\) "#d08770")
(define-ground-armory-item "Razor-Sharp Aluminum Mousepad" make-razor-sharp-aluminum-mousepad #\) "#d08770")
(define-ground-armory-item "Telescoping Pointer (Laser Inactive)" make-telescoping-pointer #\) "#d08770")
(define-ground-armory-item "Whiteboard Marker of Dominance" make-whiteboard-marker-of-dominance #\) "#d08770")
(define-ground-armory-item "Mechanical Keyboard (Cherry MX Blue)" make-mechanical-keyboard #\) "#d08770")
(define-ground-armory-item "Rubber Band Gatling Gun" make-rubber-band-gatling-gun #\) "#d08770")
(define-ground-armory-item "Nerf Retaliator (Office Modded)" make-nerf-retaliator #\) "#d08770")
(define-ground-armory-item "Can of Compressed Air" make-can-of-compressed-air #\) "#d08770")
(define-ground-armory-item "USB Drive Shuriken" make-usb-drive-shuriken #\) "#d08770")
(define-ground-armory-item "Megaphone of \"Let's Take This Offline\"" make-megaphone-of-lets-take-this-offline #\) "#d08770")
(define-ground-armory-item "Laser Pointer of Redirection" make-laser-pointer-of-redirection #\) "#d08770")
(define-ground-armory-item "\"Reply-All\" Blunderbuss" make-reply-all-blunderbuss #\) "#d08770")
(define-ground-armory-item "HR Whistleblower" make-hr-whistleblower #\) "#d08770")
(define-ground-armory-item "Startup Green T-Shirt" make-startup-green-t-shirt #\[ "#5e81ac")
(define-ground-armory-item "Patagonia Fleece Vest" make-patagonia-fleece-vest #\[ "#5e81ac")
(define-ground-armory-item "Unwashed Hoodie" make-unwashed-hoodie #\[ "#5e81ac")
(define-ground-armory-item "Ironed Button-Down" make-ironed-button-down #\[ "#5e81ac")
(define-ground-armory-item "Headphones of Noise-Canceling" make-headphones-of-noise-canceling #\] "#b48ead")
(define-ground-armory-item "Lanyard of the VIP" make-lanyard-of-the-vip #\] "#b48ead")
(define-ground-armory-item "Blue-Light Blocking Glasses" make-blue-light-blocking-glasses #\] "#b48ead")
(define-ground-armory-item "YubiKey of Second Factors" make-yubikey-of-second-factors #\] "#b48ead")
(define-ground-armory-item "AWS Certified Solutions Architect Plaque" make-aws-certified-solutions-architect-plaque #\( "#8fbcbb")
(define-ground-armory-item "Agile Scrum Master Certificate" make-agile-scrum-master-certificate #\( "#8fbcbb")
(define-ground-armory-item "Branded Corporate Yeti Mug" make-branded-corporate-yeti-mug #\( "#8fbcbb")
(define-ground-armory-item "Stack Overflow Plagiarized Script" make-stack-overflow-plagiarized-script #\( "#8fbcbb")

;;; Rare & Legendary Loot ground-item wrappers (FUTURE_PLANS.md §15).
(define-ground-armory-item "The \"Out of Office\" Auto-Responder" make-out-of-office-auto-responder #\] "#ebcb8b")
(define-ground-armory-item "The Noise-Canceling AirPods Pro" make-airpods-pro-noise-canceling #\] "#ebcb8b")
(define-ground-armory-item "The Platinum Corporate Amex" make-platinum-corporate-amex #\] "#ebcb8b")
(define-ground-armory-item "The Pager of Dread" make-pager-of-dread #\) "#ebcb8b")
(define-ground-armory-item "The B0FH's LART" make-b0fhs-lart #\) "#d33682")
(define-ground-armory-item "The Source Code of the Universe" make-source-code-of-the-universe #\) "#d33682")
(define-ground-armory-item "The C-Suite Keycard" make-c-suite-keycard #\] "#d33682")
(define-ground-armory-item "The Golden Parachute" make-golden-parachute #\[ "#d33682")
(define-ground-armory-item "The Mechanical Keyboard of the Ancients (IBM Model M)" make-mechanical-keyboard-of-the-ancients #\) "#d33682")

(defmacro define-ground-consumable-item (name item-factory char color)
  "Define the MAKE-GROUND-* wrapper corresponding to ITEM-FACTORY for a
§17 CONSUMABLE-ITEM -- mirrors DEFINE-GROUND-ARMORY-ITEM exactly (both
ultimately just call MAKE-GROUND-EQUIPPABLE-ITEM, whose own name
predates this second, non-equippable use but whose body is generic
over any RDESCENT-ITEM PAYLOAD)."
  (let* ((item-name (symbol-name item-factory))
         (prefix-length (length "MAKE-"))
         (suffix (subseq item-name prefix-length))
         (ground-name (intern (format nil "MAKE-GROUND-~A" suffix) (symbol-package item-factory))))
    `(defun ,ground-name (x y level)
       ,(format nil "Pure factory: return a fresh GROUND-ITEM wrapping ~A." name)
       (make-ground-equippable-item x y level ,char ,name ,color (,item-factory)))))

;;; §17 Tier 1 -- cheap, common, everyday snacks.
(define-ground-consumable-item "Stale Croissant" make-stale-croissant #\! "#a3be8c")
(define-ground-consumable-item "Day-Old Breakroom Pizza" make-day-old-breakroom-pizza #\! "#a3be8c")
(define-ground-consumable-item "Someone Else's Tupperware Lunch" make-someone-elses-tupperware-lunch #\! "#a3be8c")
(define-ground-consumable-item "Happy Birthday!! Sheet Cake" make-happy-birthday-sheet-cake #\! "#a3be8c")
(define-ground-consumable-item "Handful of Free Office Almonds" make-handful-of-free-office-almonds #\! "#a3be8c")
;;; §17 Tier 2 -- the caffeine aisle.
(define-ground-consumable-item "TGIF Leftover Beer" make-tgif-leftover-beer #\! "#ebcb8b")
(define-ground-consumable-item "Breakroom Coffee (Burnt)" make-breakroom-coffee-burnt #\! "#ebcb8b")
(define-ground-consumable-item "Artisan Latte" make-artisan-latte #\! "#ebcb8b")
(define-ground-consumable-item "Quadruple Shot Espresso" make-quadruple-shot-espresso #\! "#ebcb8b")
(define-ground-consumable-item "Warm Monster Energy Drink" make-warm-monster-energy-drink #\! "#ebcb8b")
(define-ground-consumable-item "The Smart Water" make-the-smart-water #\! "#ebcb8b")
;;; §17 Tier 3 -- the hard stuff.
(define-ground-consumable-item "Discarded Adderall" make-discarded-adderall #\! "#bf616a")
(define-ground-consumable-item "Modafinil" make-modafinil #\! "#bf616a")
(define-ground-consumable-item "Dexedrine Spansule" make-dexedrine-spansule #\! "#bf616a")
(define-ground-consumable-item "Baggie of Blow (Executive Grade)" make-baggie-of-blow-executive-grade #\! "#bf616a")
(define-ground-consumable-item "Microdose Tab (LSD)" make-microdose-tab-lsd #\! "#bf616a")
(define-ground-consumable-item "Unmarked Nootropic Stack" make-unmarked-nootropic-stack #\! "#bf616a")
;;; Rare Loot's own consumable (FUTURE_PLANS.md §15).
(define-ground-consumable-item "Root Password Post-It Note" make-root-password-post-it-note #\! "#ebcb8b")

(defparameter *rdescent-corporate-badge-key-id* :corporate-badge
  "The one concrete key archetype FUTURE_PLANS.md §9 (\"Keys & Locked
Doors\") presently implements -- a keyword matched against a player's
own :KEYS-HELD GAME-STATE flag (see KEY-HELD-P) to decide whether they
may pass a locked door tagged with this same id (see TILE's own
LOCKED-KEY-ID slot, PLACE-LOCKED-DOOR). Mirrors FUTURE_PLANS.md §8's
own \"only one trap archetype implemented\" precedent -- the seam
future key archetypes plug into by adding new id/name pairs and
threading them through PLACE-LOCKED-DOOR/SPAWN-KEYS-FOR-LEVEL exactly
like this one.")

(defparameter *rdescent-corporate-badge-key-name* "Corporate Badge"
  "Human-readable display name for *RDESCENT-CORPORATE-BADGE-KEY-ID*,
used both as MAKE-GROUND-KEY's own ground-item NAME and in MOVE-
PLAYER's \"you need a ~A\" locked-door message.")

(defparameter *rdescent-locked-door-char* #\+
  "The glyph a locked-door TILE renders as (until the specific player
who unlocked it sees it rendered as ordinary floor instead -- see
DOOR-OPENED-P/RENDER-GRID) -- the traditional roguelike \"closed door\"
character, distinct from #\\# (wall) and #\\. (open floor/corridor).")

(defun make-ground-key (x y level key-id key-name)
  "Pure factory: return a fresh GROUND-ITEM at (X, Y) on LEVEL
representing KEY-NAME (e.g. \"Corporate Badge\") lying on the floor --
char #\\*, NAME KEY-NAME, MESSAGE-COLOR \"#f1c40f\" (a badge-gold),
PAYLOAD (CONS :KEY KEY-ID). GRAB-ITEM's own :KEY payload branch adds
KEY-ID to the player's :KEYS-HELD GAME-STATE flag rather than
appending anything to INVENTORY (FUTURE_PLANS.md §9's own explicit
\"avoid a growing key ring\" design note), exactly mirroring MAKE-
GROUND-KOMBUCHA/MAKE-GROUND-STOCK-OPTION's own inventory-bypassing
payload shapes. See SPAWN-KEYS-FOR-LEVEL for procedural placement,
always paired 1:1 with a LOCKED-DOOR sharing the same KEY-ID/KEY-NAME."
  (make-instance 'ground-item :x x :y y :char #\* :name key-name :level level
                              :message-color "#f1c40f" :payload (cons :key key-id)))

(defun make-ground-stock-option (x y level)
  "Pure factory: return a fresh GROUND-ITEM at (X, Y) on LEVEL
representing a Stock Option lying on the floor -- RDESCENT's gold/loot
pickup, redeemed instantly for a random windfall of RSU (this game's
loot currency, see ENTITY's RSU slot/GET-RSU) rather than occupying an
inventory slot like an ordinary RDESCENT-ITEM. CHAR #\\$, NAME \"Stock
Option\", MESSAGE-COLOR \"gold\" (the color GRAB-ITEM's cash-in
message is displayed in), PAYLOAD (:STOCK-OPTION . AMOUNT) where
AMOUNT is (1+ (RANDOM 10000)), a flat 1-10000 RSU reward freshly
rolled per spawned Stock Option (unlike a Kombucha's fixed effect)."
  (make-instance 'ground-item :x x :y y :char #\$ :name "Stock Option" :level level
                              :message-color "gold" :payload (cons :stock-option (1+ (random 10000)))))

(defparameter *rdescent-monster-rsu-drop-chance-percent* 15
  "Percent chance (out of a (RANDOM 100) roll) that a slain monster
drops a Severance Package (see MAKE-GROUND-SEVERANCE-PACKAGE/MAYBE-
DROP-MONSTER-RSU) -- deliberately small, so finding one still feels
like a bonus rather than a guaranteed part of every kill.")

(defparameter *rdescent-monster-rsu-drop-multiplier-min* 3
  "Lower bound (inclusive) of the random multiplier MONSTER-RSU-DROP-
AMOUNT applies to a slain monster's own GET-XP (this engine's existing
per-monster difficulty proxy, see ENTITY's XP slot docstring) to derive
its Severance Package's RSU amount.")

(defparameter *rdescent-monster-rsu-drop-multiplier-max* 8
  "Upper bound (inclusive) of the random multiplier MONSTER-RSU-DROP-
AMOUNT applies to a slain monster's own GET-XP -- see *RDESCENT-
MONSTER-RSU-DROP-MULTIPLIER-MIN*'s own docstring.")

(defun monster-rsu-drop-amount (xp)
  "Return a random RSU amount for a Severance Package dropped by a
slain monster whose own GET-XP is XP: (ROUND (* XP MULTIPLIER)) where
MULTIPLIER is uniformly drawn from *RDESCENT-MONSTER-RSU-DROP-
MULTIPLIER-MIN*..*RDESCENT-MONSTER-RSU-DROP-MULTIPLIER-MAX* (inclusive)
-- so a tougher monster (higher XP, this engine's existing difficulty
proxy -- see MAKE-ORC/MAKE-MIDDLE-MANAGER/MAKE-TROLL's own fixed :XP
values) drops proportionally more RSU on average, at least
*RDESCENT-MONSTER-RSU-DROP-MULTIPLIER-MIN* times its own XP and at
most *RDESCENT-MONSTER-RSU-DROP-MULTIPLIER-MAX* times it. Floored at 1
so even a 0-XP monster's own (vanishingly rare) drop is never a
meaningless 0 RSU windfall."
  (max 1 (round (* xp (+ *rdescent-monster-rsu-drop-multiplier-min*
                         (random (1+ (- *rdescent-monster-rsu-drop-multiplier-max*
                                         *rdescent-monster-rsu-drop-multiplier-min*))))))))

(defun make-ground-severance-package (x y level amount)
  "Pure factory: return a fresh GROUND-ITEM at (X, Y) on LEVEL
representing a Severance Package lying on the floor -- a slain
monster's own RSU drop (see MAYBE-DROP-MONSTER-RSU), flavored as the
corporate-downsizing counterpart to MAKE-GROUND-STOCK-OPTION's own
player-found windfall, but for an explicit AMOUNT (rolled once, by the
caller, via MONSTER-RSU-DROP-AMOUNT, proportional to the slain
monster's own difficulty) rather than MAKE-GROUND-STOCK-OPTION's own
always-random 1-10000 roll. CHAR #\\$, NAME \"Severance Package\",
MESSAGE-COLOR \"gold\", PAYLOAD (:STOCK-OPTION . AMOUNT) -- deliberately
the exact same payload shape MAKE-GROUND-STOCK-OPTION uses, so GRAB-
ITEM's existing :STOCK-OPTION branch redeems this identically without
needing a dedicated case of its own."
  (make-instance 'ground-item :x x :y y :char #\$ :name "Severance Package" :level level
                              :message-color "gold" :payload (cons :stock-option amount)))

(defun maybe-drop-monster-rsu (target)
  "Return a fresh GROUND-ITEM (see MAKE-GROUND-SEVERANCE-PACKAGE) at
TARGET's own X/Y/LEVEL if this roll (against *RDESCENT-MONSTER-RSU-
DROP-CHANCE-PERCENT*) succeeds, with its own AMOUNT proportional to
TARGET's own GET-XP (via MONSTER-RSU-DROP-AMOUNT) -- or NIL if the
roll fails (the common case, since the drop chance is deliberately
small). This is the RSU-only building block; real call sites (MOVE-
PLAYER's melee kill, APPLY-ITEM's Scroll of PIP kill, CONFUSED-ENTITY-
TURN's stumble-kill, COMPANION-AI-TURN's own attack) instead call
MAYBE-DROP-MONSTER-LOOT (RDESCENT/DUNGEON.LISP), which wraps this same
outer roll but layers in a further, rarer chance of an actual
equippable item drop (MAYBE-DROP-MONSTER-ITEM) in place of the RSU
windfall this function always produces."
  (when (< (random 100) *rdescent-monster-rsu-drop-chance-percent*)
    (make-ground-severance-package (get-x target) (get-y target) (get-level target)
                                   (monster-rsu-drop-amount (get-xp target)))))


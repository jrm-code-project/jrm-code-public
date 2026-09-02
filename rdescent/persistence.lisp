;;; -*- Lisp -*-

;;; Pure serialize/deserialize for GAME-STATE (ARCHITECTURE_PLAN.md
;;; §10, the final step of §11's Suggested Build Order) -- see
;;; SERIALIZE-GAME-STATE/DESERIALIZE-GAME-STATE below for the
;;; top-level entry points, and *RDESCENT-SAVE-FORMAT-VERSION* for the
;;; version tag every serialized blob carries.
;;;
;;; Last of several files this engine was split across (originally a
;;; single ENGINE.LISP) -- see RDESCENT/ENTITIES.LISP for the value
;;; types (ENTITY/GAME-STATE/GAME-MAP/RDESCENT-ITEM/etc.) this file
;;; walks, and RDESCENT/MECHANICS.LISP for DUNGEON-LEVEL-SNAPSHOT (the
;;; per-depth value type GAME-STATE's LEVELS FSET:MAP holds). See
;;; RDESCENT/ENTITIES.LISP's own header comment for the full file map.

(in-package "JRM-CODE-PROJECT")

(defparameter *rdescent-save-format-version* 1
  "The version tag every SERIALIZE-GAME-STATE blob carries under its
own :SAVE-FORMAT-VERSION key, checked by DESERIALIZE-GAME-STATE before
attempting to reconstruct anything else. Baked in from day one --
before any format change has actually happened -- so that a future
engine change (a new ENTITY slot, a renamed class, a different
ENCODING for some field) can bump this integer and have
DESERIALIZE-GAME-STATE reject (rather than silently misinterpret) a
save blob produced by an older version of this file, instead of
signalling some unrelated, confusing error (or worse, silently
reconstructing a subtly wrong GAME-STATE) partway through
reconstruction. Bump this whenever SERIALIZE-GAME-STATE's own output
shape changes in a way DESERIALIZE-GAME-STATE can no longer read
unambiguously; a purely additive, backward-compatible change (e.g. a
new OPT-ENTRY-style optional key an older blob simply lacks) does not
require a bump, since DESERIALIZE-GAME-STATE already treats every
missing key as its slot's own NIL/default per the ASSOC-based readers
below.")

(defun serialize-boolean (value)
  "Return 1 if VALUE is true, 0 if VALUE is NIL -- used for every
GENERALIZED BOOLEAN slot (IS-ALIVE, BLOCKS-MOVEMENT, WALKABLE) so the
serialized form never has to carry a literal Lisp T or NIL: unlike an
absent key (see OPT-ENTRY), a literal NIL value survives a real
CL-JSON ENCODE-JSON-ALIST-TO-STRING/DECODE-JSON-FROM-STRING round trip
as the *string* \"nil\" rather than as a JSON boolean or null, and a
literal T likewise becomes the string \"t\" -- neither of which
DESERIALIZE-BOOLEAN could tell apart from any other truthy string.
Plain 0/1 integers have no such ambiguity in JSON (or in this file's
own pure Lisp-to-Lisp round trip, which never actually calls CL-JSON
today but is designed so a future caller safely could)."
  (if value 1 0))

(defun deserialize-boolean (value)
  "Inverse of SERIALIZE-BOOLEAN: return NIL if VALUE is 0, T (via
NOT/ZEROP) for any other (i.e. 1) value."
  (not (zerop value)))

(defun opt-entry (key value)
  "Return a one-element alist entry (KEY . VALUE) if VALUE is
non-NIL, else NIL (an empty list, so this splices away to nothing
under APPEND) -- used by every serializer below for a slot that
legitimately has no value some of the time (ENTITY's MAX-HP/HP/NAME,
STATUS-EFFECT's MAGNITUDE, TILE's ROOM-KIND) so the serialized alist
simply omits the key entirely rather than carrying a literal (KEY .
NIL) entry. This matters for the same reason SERIALIZE-BOOLEAN exists:
a bare Lisp NIL is not reliably distinguishable from other values once
round-tripped through real JSON text, but an *absent* key is exactly
as unambiguous in JSON as it is here -- every DESERIALIZE-* reader
below uses (CDR (ASSOC KEY DATA)), which already returns NIL for a
missing KEY, so omitting the key and reading it back both naturally
agree on \"no value\"."
  (when value (list (cons key value))))

(defun deserialize-keyword (value)
  "Return VALUE as a keyword, coercing a string back into the
equivalent keyword symbol first if necessary. Every SERIALIZE-*
function in this file that stores a bare Lisp keyword as a JSON
*value* (STATUS-EFFECT's KIND, ENTITY's :CLASS/FACTION/DISPOSITION,
SHRINE-FIXTURE's SHRINE-KIND, an item's :CLASS, GROUND-ITEM PAYLOAD's
:KIND/bare-keyword shapes) hits the same CL-JSON round-trip quirk:
CL-JSON:ENCODE-JSON-* has no native JSON type for a Lisp symbol, so it
always turns a keyword *value* (as opposed to an alist *key*, which
CL-JSON always turns back into a keyword on decode regardless) into a
camelCase JSON string via CL-JSON:LISP-TO-CAMEL-CASE (e.g. :GROUND-ITEM
becomes \"groundItem\", not \"ground-item\" or \"GROUND-ITEM\"), and
CL-JSON:DECODE-JSON-FROM-STRING decodes any JSON string value back as
a plain Lisp STRING verbatim (unlike an object *key*, decoding a
string *value* never runs CL-JSON:CAMEL-CASE-TO-LISP on it) -- so a
value serialized as, say, :HOSTILE or :GROUND-ITEM survives a direct
Lisp-to-Lisp SERIALIZE-*/DESERIALIZE-* round trip (this file's own
tests, which never actually call CL-JSON) as the keyword :HOSTILE/
:GROUND-ITEM unchanged, but survives a *real* PACK-SAVE-STATE/
UNPACK-SAVE-STATE round trip (which does) as the camelCase string
\"hostile\"/\"groundItem\" instead -- silently breaking any
EQL/EQ/CASE dispatch downstream (ENTITY-DISPOSITION-TOWARD,
DESERIALIZE-ENTITY-FROM-CLASS-TAG, MAKE-ITEM-FROM-CLASS-TAG,
TICK-STATUS-EFFECTS, the shrine/ground-item interaction logic in
ACTIONS.LISP) that expects an EQ-comparable keyword, not a STRING=-
comparable string. Calling this on every such value before using it
handles both paths uniformly: a value that is already a keyword (the
direct Lisp-to-Lisp path, or a value this same call already
normalized) is returned unchanged; a string (the real JSON path) is
run through CL-JSON:CAMEL-CASE-TO-LISP (the same function CL-JSON
itself already applies to every object *key* on decode) and interned
into the KEYWORD package, exactly recovering the original keyword."
  (etypecase value
    (keyword value)
    (string (intern (cl-json:camel-case-to-lisp value) :keyword))))

;;; ------------------------------------------------------------------
;;; STATUS-EFFECT

(defun serialize-status-effect (effect)
  "Return an alist capturing EFFECT (a STATUS-EFFECT instance)'s KIND/
TICKS-REMAINING/MAGNITUDE, suitable for DESERIALIZE-STATUS-EFFECT to
reconstruct an EQL STATUS-EFFECT from. MAGNITUDE is only included (via
OPT-ENTRY) when non-NIL, matching STATUS-EFFECT's own \"NIL means this
KIND has no per-tick numeric effect\" convention (see TICK-STATUS-
EFFECTS)."
  (list* (cons :kind (status-effect-kind effect))
         (cons :ticks-remaining (status-effect-ticks-remaining effect))
         (opt-entry :magnitude (status-effect-magnitude effect))))

(defun deserialize-status-effect (data)
  "Inverse of SERIALIZE-STATUS-EFFECT: return a fresh STATUS-EFFECT
reconstructed from DATA (an alist in SERIALIZE-STATUS-EFFECT's own
shape)."
  (make-instance 'status-effect
                 :kind (deserialize-keyword (cdr (assoc :kind data)))
                 :ticks-remaining (cdr (assoc :ticks-remaining data))
                 :magnitude (cdr (assoc :magnitude data))))

;;; ------------------------------------------------------------------
;;; RDESCENT-ITEM hierarchy
;;;
;;; Every concrete RDESCENT-ITEM class today (SCROLL-OF-PIP/REPLY-ALL-
;;; BOMB/REORG-MEMO/STACK-OF-UNREAD-MEMOS) is a stateless leaf: all of
;;; NAME/EQUIP-SLOT/STAT-BONUSES/etc. come from fixed :DEFAULT-INITARGS
;;; on the class itself (see ENTITIES.LISP) -- except for EQUIPPABLE-
;;; ITEM's own MODIFIER (:NORMAL/:CURSED/:BLESSED, see ITEM-MODIFIER),
;;; which genuinely is per-instance state that could vary from one
;;; Stack of Unread Memos to another, rolled randomly at the moment an
;;; item is actually spawned into the game world (see RANDOMIZE-
;;; EQUIPPABLE-ITEM-MODIFIER). So SERIALIZE-ITEM/MAKE-ITEM-FROM-CLASS-
;;; TAG carry MODIFIER as an explicit extra key/argument alongside the
;;; class tag, while every other slot still comes for free from the
;;; matching MAKE-* factory's own fixed :DEFAULT-INITARGS.

(defgeneric item-class-tag (item)
  (:documentation
   "Return the keyword tag (:SCROLL-OF-PIP/:REPLY-ALL-BOMB/:REORG-MEMO/
:STACK-OF-UNREAD-MEMOS plus the concrete §13 armory item tags)
identifying ITEM's concrete class, for
SERIALIZE-ITEM. Signals an error for any other RDESCENT-ITEM subclass,
which would need a new TAG here (and a new case in DESERIALIZE-ITEM)
before it could be persisted.")
  (:method ((item reorg-memo))
    :reorg-memo)
  (:method ((item reply-all-bomb))
    :reply-all-bomb)
  (:method ((item scroll-of-pip))
    :scroll-of-pip)
  (:method ((item stack-of-unread-memos))
    :stack-of-unread-memos)
  (:method ((item rdescent-item))
    (error "ITEM-CLASS-TAG: unrecognized item class ~S" (class-of item))))

(defun serialize-item (item)
  "Return an alist tagging ITEM's concrete class (see ITEM-CLASS-TAG)
plus its own ITEM-MODIFIER (see ENTITIES.LISP) -- for a plain
(non-EQUIPPABLE-ITEM) item ITEM-MODIFIER's own default :METHOD always
answers :NORMAL, so this key is included uniformly for every
RDESCENT-ITEM rather than only for EQUIPPABLE-ITEM ones. :CLOAKED (see
ITEM-CLOAKED-P) is likewise included uniformly (a plain item's own
default :METHOD always answers NIL) so a previously-equipped
EQUIPPABLE-ITEM's own permanently-revealed MODIFIER prefix survives a
save/load round-trip. :DURABILITY/
:MAX-DURABILITY (via OPT-ENTRY, so a plain item with no notion of
durability at all -- GET-DURABILITY/GET-MAX-DURABILITY's own default
:METHOD answers NIL -- omits both keys entirely rather than persisting
a meaningless NIL) preserve an EQUIPPABLE-ITEM's own current wear."
  (append (list (cons :class (item-class-tag item))
                (cons :modifier (item-modifier item))
                (cons :cloaked (serialize-boolean (item-cloaked-p item))))
          (opt-entry :durability (get-durability item))
          (opt-entry :max-durability (get-max-durability item))))

(defgeneric make-item-from-class-tag (tag &optional modifier durability max-durability cloaked)
  (:documentation
   "Return a fresh RDESCENT-ITEM of the concrete class TAG names, via
that class's own MAKE-* factory (MAKE-SCROLL-OF-PIP/MAKE-REPLY-ALL-
BOMB/MAKE-REORG-MEMO/MAKE-STACK-OF-UNREAD-MEMOS and the §13 armory
factories). MODIFIER (default :NORMAL) is only meaningful for an
EQUIPPABLE-ITEM tag -- it is passed straight through as that factory's
own :MODIFIER keyword argument (see ITEM-WITH-MODIFIER/ENTITIES.LISP).
DURABILITY/MAX-DURABILITY (both default NIL, meaning \"use that
factory's own deterministic default\" -- see EQUIPPABLE-ITEM's own
class docstring) are likewise only forwarded to an EQUIPPABLE-ITEM
factory's own &KEY DURABILITY/MAX-DURABILITY when explicitly supplied
(DESERIALIZE-ITEM, restoring a previously worn item's exact
durability, is the only real caller that ever supplies them); CLOAKED
(default T, matching a freshly spawned item's own class default) is
only meaningful for an EQUIPPABLE-ITEM tag too -- it is passed straight
through as that factory's own :CLOAKED keyword argument (see
ITEM-UNCLOAKED/ENTITIES.LISP), so DESERIALIZE-ITEM can restore a
previously-equipped item's permanently-revealed state; every
non-EQUIPPABLE-ITEM tag's own :METHOD simply ignores all four, since
those factories take no arguments at all.")
  (:method ((tag (eql :scroll-of-pip)) &optional (modifier :normal) durability max-durability (cloaked t))
    (declare (ignore modifier durability max-durability cloaked))
    (make-scroll-of-pip))
  (:method ((tag (eql :reply-all-bomb)) &optional (modifier :normal) durability max-durability (cloaked t))
    (declare (ignore modifier durability max-durability cloaked))
    (make-reply-all-bomb))
  (:method ((tag (eql :reorg-memo)) &optional (modifier :normal) durability max-durability (cloaked t))
    (declare (ignore modifier durability max-durability cloaked))
    (make-reorg-memo))
  (:method ((tag (eql :stack-of-unread-memos)) &optional (modifier :normal) durability max-durability (cloaked t))
    (apply #'make-stack-of-unread-memos :modifier modifier :cloaked cloaked
           (append (when max-durability (list :max-durability max-durability))
                   (when durability (list :durability durability)))))
  (:method ((tag symbol) &optional modifier durability max-durability cloaked)
    (declare (ignore modifier durability max-durability cloaked))
    (error "MAKE-ITEM-FROM-CLASS-TAG: unrecognized item class tag ~S" tag)))

(defmacro define-armory-item-persistence (class tag factory)
  "Define the ITEM-CLASS-TAG and MAKE-ITEM-FROM-CLASS-TAG methods for
one §13 equippable item. Every concrete §13 armory class is an
EQUIPPABLE-ITEM (see DEFINE-ARMORY-EQUIPPABLE-ITEM), so FACTORY always
accepts the same &KEY MODIFIER/MAX-DURABILITY/DURABILITY/CLOAKED every
such generated factory does; MAX-DURABILITY/DURABILITY are only
forwarded when DESERIALIZE-ITEM (the only real caller that ever
supplies them) passes them non-NIL, so a fresh (non-deserialized) item
still gets that factory's own deterministic default."
  `(progn
     (defmethod item-class-tag ((item ,class))
       ,tag)
     (defmethod make-item-from-class-tag ((tag (eql ,tag)) &optional (modifier :normal) durability max-durability (cloaked t))
       (apply #',factory :modifier modifier :cloaked cloaked
              (append (when max-durability (list :max-durability max-durability))
                      (when durability (list :durability durability)))))))

(define-armory-item-persistence keyboard-of-kinesis :keyboard-of-kinesis make-keyboard-of-kinesis)
(define-armory-item-persistence red-swingline-stapler :red-swingline-stapler make-red-swingline-stapler)
(define-armory-item-persistence three-foot-ethernet-cable :three-foot-ethernet-cable make-three-foot-ethernet-cable)
(define-armory-item-persistence severed-server-rack-rail :severed-server-rack-rail make-severed-server-rack-rail)
(define-armory-item-persistence razor-sharp-aluminum-mousepad :razor-sharp-aluminum-mousepad make-razor-sharp-aluminum-mousepad)
(define-armory-item-persistence telescoping-pointer :telescoping-pointer make-telescoping-pointer)
(define-armory-item-persistence whiteboard-marker-of-dominance :whiteboard-marker-of-dominance make-whiteboard-marker-of-dominance)
(define-armory-item-persistence mechanical-keyboard :mechanical-keyboard make-mechanical-keyboard)
(define-armory-item-persistence rubber-band-gatling-gun :rubber-band-gatling-gun make-rubber-band-gatling-gun)
(define-armory-item-persistence nerf-retaliator :nerf-retaliator make-nerf-retaliator)
(define-armory-item-persistence can-of-compressed-air :can-of-compressed-air make-can-of-compressed-air)
(define-armory-item-persistence usb-drive-shuriken :usb-drive-shuriken make-usb-drive-shuriken)
(define-armory-item-persistence megaphone-of-lets-take-this-offline :megaphone-of-lets-take-this-offline make-megaphone-of-lets-take-this-offline)
(define-armory-item-persistence laser-pointer-of-redirection :laser-pointer-of-redirection make-laser-pointer-of-redirection)
(define-armory-item-persistence reply-all-blunderbuss :reply-all-blunderbuss make-reply-all-blunderbuss)
(define-armory-item-persistence hr-whistleblower :hr-whistleblower make-hr-whistleblower)
(define-armory-item-persistence startup-green-t-shirt :startup-green-t-shirt make-startup-green-t-shirt)
(define-armory-item-persistence patagonia-fleece-vest :patagonia-fleece-vest make-patagonia-fleece-vest)
(define-armory-item-persistence unwashed-hoodie :unwashed-hoodie make-unwashed-hoodie)
(define-armory-item-persistence ironed-button-down :ironed-button-down make-ironed-button-down)
(define-armory-item-persistence headphones-of-noise-canceling :headphones-of-noise-canceling make-headphones-of-noise-canceling)
(define-armory-item-persistence lanyard-of-the-vip :lanyard-of-the-vip make-lanyard-of-the-vip)
(define-armory-item-persistence blue-light-blocking-glasses :blue-light-blocking-glasses make-blue-light-blocking-glasses)
(define-armory-item-persistence yubikey-of-second-factors :yubikey-of-second-factors make-yubikey-of-second-factors)
(define-armory-item-persistence aws-certified-solutions-architect-plaque :aws-certified-solutions-architect-plaque make-aws-certified-solutions-architect-plaque)
(define-armory-item-persistence agile-scrum-master-certificate :agile-scrum-master-certificate make-agile-scrum-master-certificate)
(define-armory-item-persistence branded-corporate-yeti-mug :branded-corporate-yeti-mug make-branded-corporate-yeti-mug)
(define-armory-item-persistence stack-overflow-plagiarized-script :stack-overflow-plagiarized-script make-stack-overflow-plagiarized-script)

;;; Rare & Legendary Loot (FUTURE_PLANS.md §15)
(define-armory-item-persistence out-of-office-auto-responder :out-of-office-auto-responder make-out-of-office-auto-responder)
(define-armory-item-persistence airpods-pro-noise-canceling :airpods-pro-noise-canceling make-airpods-pro-noise-canceling)
(define-armory-item-persistence platinum-corporate-amex :platinum-corporate-amex make-platinum-corporate-amex)
(define-armory-item-persistence pager-of-dread :pager-of-dread make-pager-of-dread)
(define-armory-item-persistence b0fhs-lart :b0fhs-lart make-b0fhs-lart)
(define-armory-item-persistence source-code-of-the-universe :source-code-of-the-universe make-source-code-of-the-universe)
(define-armory-item-persistence c-suite-keycard :c-suite-keycard make-c-suite-keycard)
(define-armory-item-persistence golden-parachute :golden-parachute make-golden-parachute)
(define-armory-item-persistence mechanical-keyboard-of-the-ancients :mechanical-keyboard-of-the-ancients make-mechanical-keyboard-of-the-ancients)

(defmacro define-consumable-item-persistence (class tag factory)
  "Define the ITEM-CLASS-TAG and MAKE-ITEM-FROM-CLASS-TAG methods for
one §17 CONSUMABLE-ITEM. Every concrete CONSUMABLE-ITEM (see
DEFINE-CONSUMABLE-ITEM/RDESCENT/ENTITIES.LISP) is a stateless leaf --
none of them carry any per-instance MODIFIER/DURABILITY the way an
EQUIPPABLE-ITEM does -- so FACTORY is simply called with no
arguments, mirroring SCROLL-OF-PIP/REPLY-ALL-BOMB/REORG-MEMO's own
ITEM-CLASS-TAG/MAKE-ITEM-FROM-CLASS-TAG methods above rather than
DEFINE-ARMORY-ITEM-PERSISTENCE's MODIFIER/DURABILITY-forwarding one."
  `(progn
     (defmethod item-class-tag ((item ,class))
       ,tag)
     (defmethod make-item-from-class-tag ((tag (eql ,tag)) &optional (modifier :normal) durability max-durability (cloaked t))
       (declare (ignore modifier durability max-durability cloaked))
       (funcall #',factory))))

(define-consumable-item-persistence stale-croissant :stale-croissant make-stale-croissant)
(define-consumable-item-persistence day-old-breakroom-pizza :day-old-breakroom-pizza make-day-old-breakroom-pizza)
(define-consumable-item-persistence someone-elses-tupperware-lunch :someone-elses-tupperware-lunch make-someone-elses-tupperware-lunch)
(define-consumable-item-persistence happy-birthday-sheet-cake :happy-birthday-sheet-cake make-happy-birthday-sheet-cake)
(define-consumable-item-persistence handful-of-free-office-almonds :handful-of-free-office-almonds make-handful-of-free-office-almonds)
(define-consumable-item-persistence tgif-leftover-beer :tgif-leftover-beer make-tgif-leftover-beer)
(define-consumable-item-persistence breakroom-coffee-burnt :breakroom-coffee-burnt make-breakroom-coffee-burnt)
(define-consumable-item-persistence artisan-latte :artisan-latte make-artisan-latte)
(define-consumable-item-persistence quadruple-shot-espresso :quadruple-shot-espresso make-quadruple-shot-espresso)
(define-consumable-item-persistence warm-monster-energy-drink :warm-monster-energy-drink make-warm-monster-energy-drink)
(define-consumable-item-persistence the-smart-water :the-smart-water make-the-smart-water)
(define-consumable-item-persistence discarded-adderall :discarded-adderall make-discarded-adderall)
(define-consumable-item-persistence modafinil :modafinil make-modafinil)
(define-consumable-item-persistence dexedrine-spansule :dexedrine-spansule make-dexedrine-spansule)
(define-consumable-item-persistence baggie-of-blow-executive-grade :baggie-of-blow-executive-grade make-baggie-of-blow-executive-grade)
(define-consumable-item-persistence microdose-tab-lsd :microdose-tab-lsd make-microdose-tab-lsd)
(define-consumable-item-persistence unmarked-nootropic-stack :unmarked-nootropic-stack make-unmarked-nootropic-stack)
(define-consumable-item-persistence root-password-post-it-note :root-password-post-it-note make-root-password-post-it-note)

(defun deserialize-item (data)
  "Inverse of SERIALIZE-ITEM: return a fresh RDESCENT-ITEM of the
concrete class DATA's :CLASS tag names, via that class's own MAKE-*
factory (MAKE-SCROLL-OF-PIP/MAKE-REPLY-ALL-BOMB/MAKE-REORG-MEMO/
MAKE-STACK-OF-UNREAD-MEMOS), reconstructed with DATA's own :MODIFIER
key (defaulting to :NORMAL, via DESERIALIZE-KEYWORD, when DATA lacks
the key entirely -- backward-compatible with any save blob serialized
before MODIFIER existed), its own :DURABILITY/:MAX-DURABILITY keys
(defaulting to NIL -- \"use that item's own class default\" -- when
DATA lacks them, backward-compatible with any save blob serialized
before DURABILITY existed), and its own :CLOAKED key (via
DESERIALIZE-BOOLEAN, defaulting to T -- \"still cloaked\" -- when DATA
lacks the key entirely, backward-compatible with any save blob
serialized before CLOAKED existed, since every item that predates
cloaking was effectively spawned before the player could have equipped
it under this new rule)."
  (make-item-from-class-tag (deserialize-keyword (cdr (assoc :class data)))
                             (let ((modifier (cdr (assoc :modifier data))))
                               (if modifier (deserialize-keyword modifier) :normal))
                             (cdr (assoc :durability data))
                             (cdr (assoc :max-durability data))
                             (let ((cloaked (assoc :cloaked data)))
                               (if cloaked (deserialize-boolean (cdr cloaked)) t))))

(defun serialize-equipment (equipment)
  "Return EQUIPMENT (an ENTITY equipment plist) as an alist suitable
for JSON-safe persistence: each slot key is preserved and each
equipped item value is serialized through SERIALIZE-ITEM rather than
left as a raw CLOS instance."
  (loop for (slot item) on equipment by #'cddr
        collect (cons slot (serialize-item item))))

(defun deserialize-equipment (data)
  "Inverse of SERIALIZE-EQUIPMENT: return an equipment plist whose slot
keys and item payloads have both been reconstructed from DATA."
  (loop for (slot . item-data) in data
        append (list (deserialize-keyword slot)
                     (deserialize-item item-data))))

(defun serialize-payload (payload)
  "Return an alist capturing PAYLOAD, a GROUND-ITEM's own PAYLOAD slot
(see GROUND-ITEM's docstring for the shapes PAYLOAD can take): an
RDESCENT-ITEM instance (tagged :KIND :ITEM, with the item itself under
:ITEM via SERIALIZE-ITEM), a (:STOCK-OPTION . AMOUNT) cons (tagged
:KIND :STOCK-OPTION, with AMOUNT under :AMOUNT), a (:KEY . KEY-ID) cons
(FUTURE_PLANS.md §9, tagged :KIND :KEY, with KEY-ID under :KEY-ID), or
a bare keyword such as :KOMBUCHA (in which case :KIND is simply that
keyword itself, and DESERIALIZE-PAYLOAD's fallback CASE clause hands
it back unchanged) -- see DESERIALIZE-PAYLOAD for the inverse."
  (cond
    ((typep payload 'rdescent-item)
     (list (cons :kind :item) (cons :item (serialize-item payload))))
    ((and (consp payload) (eq (car payload) :stock-option))
     (list (cons :kind :stock-option) (cons :amount (cdr payload))))
    ((and (consp payload) (eq (car payload) :key))
     (list (cons :kind :key) (cons :key-id (cdr payload))))
    ((keywordp payload) (list (cons :kind payload)))
    (t (error "SERIALIZE-PAYLOAD: unrecognized GROUND-ITEM payload shape ~S" payload))))

(defun deserialize-payload (data)
  "Inverse of SERIALIZE-PAYLOAD: reconstruct whichever of the GROUND-
ITEM PAYLOAD shapes DATA's :KIND tag names. :KIND (and the bare
keyword itself, in the fallback case) is normalized via DESERIALIZE-
KEYWORD, since a real PACK-SAVE-STATE/UNPACK-SAVE-STATE round trip
hands this back as a lowercase string, not the original keyword (see
DESERIALIZE-KEYWORD's own docstring)."
  (let ((kind (deserialize-keyword (cdr (assoc :kind data)))))
    (case kind
      (:item (deserialize-item (cdr (assoc :item data))))
      (:stock-option (cons :stock-option (cdr (assoc :amount data))))
      (:key (cons :key (deserialize-keyword (cdr (assoc :key-id data)))))
      (t kind))))

;;; ------------------------------------------------------------------
;;; ENTITY hierarchy (ENTITY/ENEMY/GROUND-ITEM/FIXTURE/SHRINE-FIXTURE)
;;;
;;; ENEMY carries no slots of its own beyond ENTITY's (see ENEMY's
;;; docstring) -- its only wrinkle is an INITIALIZE-INSTANCE :AFTER
;;; that auto-derives XP/DISPOSITION from other initargs *when they are
;;; not themselves supplied*. DESERIALIZE-ENTITY always supplies both
;;; explicitly (as part of COMMON-INITARGS below, read straight from
;;; DATA), so that :AFTER method's OWN (GETF INITARGS :XP)-style check
;;; always finds them already present and never overrides the
;;; serialized value with a freshly re-derived one -- an ENEMY that had
;;; its auto-derived XP/DISPOSITION overwritten by some other code path
;;; after construction (nothing does this today, but nothing prevents
;;; it either) still round-trips its *actual* current value, not a
;;; recomputed one.

(defgeneric entity-class-tag (ent)
  (:documentation "Return the keyword tag (:SHRINE-FIXTURE/:VENDOR-FIXTURE/:NPC-FIXTURE/:PLAQUE-FIXTURE/
:GROUND-ITEM/:COMPANION/:AUTO-PICKUP-ITEM/:ENEMY/:ENTITY) identifying ENT's concrete class, for SERIALIZE-ENTITY.
Dispatches most-specific-subclass first (SHRINE-FIXTURE/VENDOR-FIXTURE/NPC-FIXTURE/PLAQUE-FIXTURE before their own
FIXTURE superclass, which -- like ENTITY itself -- is only ever a
fallback here since no other ENTITY subclass exists in the bestiary
today; see this file's own header comment) so a SHRINE-FIXTURE,
VENDOR-FIXTURE, NPC-FIXTURE, PLAQUE-FIXTURE, GROUND-ITEM, COMPANION, or AUTO-PICKUP-ITEM is never mistagged as a plain :ENTITY. ORC/
TROLL (see ENTITIES.LISP) have no method of their own and fall through
to the ENEMY method -- a troll or orc's specific subclass is
deliberately not preserved across a save/restore round trip, exactly
as when this was a TYPEP COND chain.")
  (:method ((ent shrine-fixture)) :shrine-fixture)
  (:method ((ent vendor-fixture)) :vendor-fixture)
  (:method ((ent npc-fixture)) :npc-fixture)
  (:method ((ent plaque-fixture)) :plaque-fixture)
  (:method ((ent ground-item)) :ground-item)
  (:method ((ent companion)) :companion)
  (:method ((ent auto-pickup-item)) :auto-pickup-item)
  (:method ((ent enemy)) :enemy)
  (:method ((ent entity)) :entity))

(defgeneric entity-subclass-extra-alist (ent)
  (:documentation "Return the list of subclass-specific alist entries SERIALIZE-ENTITY
must append after ENTITY's own common slots -- PAYLOAD for a
GROUND-ITEM, SHRINE-KIND/USE-COUNT for a SHRINE-FIXTURE, NPC-KIND for
an NPC-FIXTURE, PLAQUE-TEXT for a PLAQUE-FIXTURE, BONDED for a
COMPANION -- or NIL for any other ENTITY subclass (including ORC/
TROLL, which add no slots of their own beyond ENEMY's). Dispatches on
ENT's own class, mirroring DESERIALIZE-ENTITY-FROM-CLASS-TAG's inverse
per-tag initargs.")
  (:method ((ent entity)) nil)
  (:method ((ent ground-item))
    (list (cons :payload (serialize-payload (get-payload ent)))))
  (:method ((ent shrine-fixture))
    (list (cons :shrine-kind (get-shrine-kind ent))
          (cons :use-count (get-use-count ent))))
  (:method ((ent npc-fixture))
    (list (cons :npc-kind (get-npc-kind ent))))
  (:method ((ent plaque-fixture))
    (list (cons :plaque-text (get-plaque-text ent))))
  (:method ((ent companion))
    (list (cons :bonded (serialize-boolean (companion-bonded-p ent)))))
  (:method ((ent auto-pickup-item))
    (list (cons :item-id (get-item-id ent)))))

(defun serialize-entity (ent)
  "Return an alist capturing every one of ENT's slots (ENTITY's own
30, plus whichever subclass-specific slots ENT's own class implies --
PAYLOAD for a GROUND-ITEM, SHRINE-KIND/USE-COUNT for a SHRINE-FIXTURE
-- see ENTITY-SUBCLASS-EXTRA-ALIST), suitable for DESERIALIZE-ENTITY
to reconstruct an equivalent ENT from. ACTIVE-EFFECTS/INVENTORY/
EQUIPMENT are recursed into via SERIALIZE-STATUS-EFFECT/SERIALIZE-ITEM/
SERIALIZE-EQUIPMENT respectively, since each holds its own nested CLOS
instances. CHAR is stored as a one-character string (JSON has no
native character type). MAX-HP/HP/NAME/ACTIVE-EFFECTS/INVENTORY/
EQUIPMENT are only included when non-NIL (see OPT-ENTRY) -- several
ENTITY subclasses (GROUND-ITEM, FIXTURE/SHRINE-FIXTURE, the stairs
markers) never set HP/MAX-HP/NAME at all, and most ENTITYs (players
who haven't picked anything up yet, every ENEMY, every non-COMPANION)
have an empty ACTIVE-EFFECTS/INVENTORY/EQUIPMENT. All six matter for
the exact same reason: a real CL-JSON ENCODE-JSON-ALIST-TO-STRING
round trip (PACK-SAVE-STATE/UNPACK-SAVE-STATE) cannot tell a bare Lisp
NIL value apart from JSON false/null, so it would otherwise round-trip
as the literal string \"nil\" rather than an absent key or an empty
array/object -- omitting the key entirely (via OPT-ENTRY) sidesteps
that ambiguity exactly as it already does for MAX-HP/HP/NAME, and
DESERIALIZE-ENTITY's own ASSOC-based reads already treat a missing key
as NIL, matching MAPCAR/ALIST-PLIST's own \"NIL in, NIL out\" behavior
for an absent ACTIVE-EFFECTS/INVENTORY/EQUIPMENT list."
  (append
   (list (cons :class (entity-class-tag ent))
         (cons :x (get-x ent))
         (cons :y (get-y ent))
         (cons :char (string (get-char ent)))
         (cons :level (get-level ent))
         (cons :blocks-movement (serialize-boolean (get-blocks-movement ent)))
         (cons :defense (defense ent))
         (cons :power (power ent))
         (cons :render-order (render-order ent))
         (cons :is-alive (serialize-boolean (is-alive ent)))
         (cons :energy (entity-energy ent))
         (cons :speed (entity-speed ent))
         (cons :heal-progress (entity-heal-progress ent))
         (cons :xp (get-xp ent))
         (cons :message-color (entity-message-color ent))
         (cons :kombucha (get-kombucha ent))
         (cons :rsu (get-rsu ent))
         (cons :bandwidth (get-bandwidth ent))
         (cons :pivot (get-pivot ent))
         (cons :caffeine-tolerance (get-caffeine-tolerance ent))
         (cons :domain-knowledge (get-domain-knowledge ent))
         (cons :seniority (get-seniority ent))
         (cons :synergy (get-synergy ent))
         (cons :hygiene (get-hygiene ent))
         (cons :faction (get-faction ent))
         (cons :disposition (get-disposition ent)))
   (opt-entry :max-hp (max-hp ent))
   (opt-entry :hp (hp ent))
   (opt-entry :name (get-name ent))
   (opt-entry :active-effects (mapcar #'serialize-status-effect (get-active-effects ent)))
   (opt-entry :inventory (mapcar #'serialize-item (get-inventory ent)))
   (opt-entry :equipment (serialize-equipment (get-equipment ent)))
   (opt-entry :collection-log (get-collection-log ent))
   (entity-subclass-extra-alist ent)))


(defgeneric deserialize-entity-from-class-tag (tag data common-initargs)
  (:documentation "Return a fresh ENTITY (or ENEMY/GROUND-ITEM/SHRINE-FIXTURE/VENDOR-FIXTURE, per TAG)
reconstructed from DATA (an alist in SERIALIZE-ENTITY's own shape) and COMMON-INITARGS (the list of initargs every subclass shares, read straight from DATA). TAG is the :CLASS keyword read from DATA, so this dispatches to the right MAKE-INSTANCE call for that subclass, supplying any subclass-specific initargs (PAYLOAD for a GROUND-ITEM, SHRINE-KIND/USE-COUNT for a SHRINE-FIXTURE, BONDED for a COMPANION) read from DATA as well -- VENDOR-FIXTURE has no extra slots of its own (its stock is the single shared *RDESCENT-VENDOR-STOCK-TABLE* constant, not per-instance state), so its own method supplies none.")
  (:method ((tag (eql :entity)) data common-initargs)
    (apply #'make-instance 'entity common-initargs))
  (:method ((tag (eql :enemy)) data common-initargs)
    (apply #'make-instance 'enemy common-initargs))
  (:method ((tag (eql :ground-item)) data common-initargs)
    (apply #'make-instance 'ground-item
           :payload (deserialize-payload (cdr (assoc :payload data)))
           common-initargs))
  (:method ((tag (eql :shrine-fixture)) data common-initargs)
    (apply #'make-instance 'shrine-fixture
           :shrine-kind (deserialize-keyword (cdr (assoc :shrine-kind data)))
           :use-count (cdr (assoc :use-count data))
           common-initargs))
  (:method ((tag (eql :vendor-fixture)) data common-initargs)
    (declare (ignore data))
    (apply #'make-instance 'vendor-fixture common-initargs))
  (:method ((tag (eql :npc-fixture)) data common-initargs)
    (apply #'make-instance 'npc-fixture
           :npc-kind (deserialize-keyword (cdr (assoc :npc-kind data)))
           common-initargs))
  (:method ((tag (eql :plaque-fixture)) data common-initargs)
    (apply #'make-instance 'plaque-fixture
           :plaque-text (cdr (assoc :plaque-text data))
           common-initargs))
  (:method ((tag (eql :companion)) data common-initargs)
    (apply #'make-instance 'companion
           :bonded (deserialize-boolean (cdr (assoc :bonded data)))
           common-initargs))
  (:method ((tag (eql :auto-pickup-item)) data common-initargs)
    (apply #'make-instance 'auto-pickup-item
           :item-id (deserialize-keyword (cdr (assoc :item-id data)))
           common-initargs))
  (:method ((tag symbol) data common-initargs)
    (error "DESERIALIZE-ENTITY-FROM-CLASS-TAG: unrecognized entity class tag ~S" tag)))

(defun deserialize-entity (data)
  "Inverse of SERIALIZE-ENTITY: return a fresh ENTITY (or ENEMY/
GROUND-ITEM/SHRINE-FIXTURE, per DATA's own :CLASS tag) reconstructed
from DATA (an alist in SERIALIZE-ENTITY's own shape)."
  (let* ((tag (deserialize-keyword (cdr (assoc :class data))))
         (common-initargs
           (list :x (cdr (assoc :x data))
                 :y (cdr (assoc :y data))
                 :char (char (cdr (assoc :char data)) 0)
                 :level (cdr (assoc :level data))
                 :name (cdr (assoc :name data))
                 :blocks-movement (deserialize-boolean (cdr (assoc :blocks-movement data)))
                 :max-hp (cdr (assoc :max-hp data))
                 :hp (cdr (assoc :hp data))
                 :defense (cdr (assoc :defense data))
                 :power (cdr (assoc :power data))
                 :render-order (cdr (assoc :render-order data))
                 :is-alive (deserialize-boolean (cdr (assoc :is-alive data)))
                 :energy (cdr (assoc :energy data))
                 :speed (cdr (assoc :speed data))
                 :heal-progress (cdr (assoc :heal-progress data))
                 :active-effects (mapcar #'deserialize-status-effect (cdr (assoc :active-effects data)))
                 :xp (cdr (assoc :xp data))
                 :message-color (cdr (assoc :message-color data))
                 :kombucha (cdr (assoc :kombucha data))
                 :rsu (cdr (assoc :rsu data))
                 :bandwidth (cdr (assoc :bandwidth data))
                 :pivot (cdr (assoc :pivot data))
                 :caffeine-tolerance (cdr (assoc :caffeine-tolerance data))
                 :domain-knowledge (cdr (assoc :domain-knowledge data))
                 :seniority (cdr (assoc :seniority data))
                 :synergy (cdr (assoc :synergy data))
                 :hygiene (cdr (assoc :hygiene data))
                 :faction (deserialize-keyword (cdr (assoc :faction data)))
                 :disposition (deserialize-keyword (cdr (assoc :disposition data)))
                 :inventory (mapcar #'deserialize-item (cdr (assoc :inventory data)))
                 :equipment (deserialize-equipment (cdr (assoc :equipment data)))
                 :collection-log (mapcar #'deserialize-keyword (cdr (assoc :collection-log data))))))
    (deserialize-entity-from-class-tag tag data common-initargs)))

;;; ------------------------------------------------------------------
;;; TILE / GAME-MAP

(defun serialize-tile (tile)
  "Return an alist capturing TILE's WALKABLE/CHAR/ROOM-KIND/
LOCKED-KEY-ID/LOCKED-KEY-NAME, suitable for DESERIALIZE-TILE to
reconstruct an EQUAL TILE from. ROOM-KIND/LOCKED-KEY-ID/LOCKED-KEY-
NAME are only included (via OPT-ENTRY) when non-NIL -- an ordinary
tile's own NIL ROOM-KIND/LOCKED-KEY-ID/LOCKED-KEY-NAME (see TILE's
docstring) should survive the round trip as NIL, not a literal encoded
null."
  (list* (cons :walkable (serialize-boolean (get-walkable tile)))
         (cons :char (string (get-char tile)))
         (append (opt-entry :room-kind (get-room-kind tile))
                 (opt-entry :locked-key-id (get-locked-key-id tile))
                 (opt-entry :locked-key-name (get-locked-key-name tile)))))

(defun deserialize-tile (data)
  "Inverse of SERIALIZE-TILE: return a fresh TILE reconstructed from
DATA (an alist in SERIALIZE-TILE's own shape). A non-NIL ROOM-KIND/
LOCKED-KEY-ID is normalized via DESERIALIZE-KEYWORD, since a real
PACK-SAVE-STATE/UNPACK-SAVE-STATE round trip hands it back as a
lowercase string, not the original keyword (see DESERIALIZE-KEYWORD's
own docstring); a NIL ROOM-KIND/LOCKED-KEY-ID (the OPT-ENTRY-omitted
case) is left as NIL rather than passed to DESERIALIZE-KEYWORD, which
only handles an already-a-keyword or a string, not NIL (a plain
CL:NIL, not a KEYWORD package symbol). LOCKED-KEY-NAME is an ordinary
display string, not a keyword, so it round-trips as-is (still via
OPT-ENTRY-omitted-means-NIL, but with no DESERIALIZE-KEYWORD call)."
  (make-instance 'tile
                 :walkable (deserialize-boolean (cdr (assoc :walkable data)))
                 :char (char (cdr (assoc :char data)) 0)
                 :room-kind (let ((rk (cdr (assoc :room-kind data))))
                              (and rk (deserialize-keyword rk)))
                 :locked-key-id (let ((kid (cdr (assoc :locked-key-id data))))
                                  (and kid (deserialize-keyword kid)))
                 :locked-key-name (cdr (assoc :locked-key-name data))))

(defun serialize-game-map (map)
  "Return an alist capturing MAP's TILES 2D array as :HEIGHT/:WIDTH
dimensions plus :ROWS, a list of HEIGHT rows each a list of WIDTH
SERIALIZE-TILE results (row-major, matching TILES' own (AREF TILES Y
X) indexing -- see GAME-MAP's docstring), suitable for
DESERIALIZE-GAME-MAP to reconstruct an equivalent MAP from."
  (let* ((tiles (get-tiles map))
         (height (array-dimension tiles 0))
         (width (array-dimension tiles 1)))
    (list (cons :height height)
          (cons :width width)
          (cons :rows (loop for y below height
                             collect (loop for x below width
                                           collect (serialize-tile (aref tiles y x))))))))

(defun deserialize-game-map (data)
  "Inverse of SERIALIZE-GAME-MAP: return a fresh GAME-MAP reconstructed
from DATA (an alist in SERIALIZE-GAME-MAP's own shape)."
  (let* ((height (cdr (assoc :height data)))
         (width (cdr (assoc :width data)))
         (tiles (make-array (list height width))))
    (loop for y below height
          for row in (cdr (assoc :rows data))
          do (loop for x below width
                   for tile-data in row
                   do (setf (aref tiles y x) (deserialize-tile tile-data))))
    (make-instance 'game-map :tiles tiles)))

;;; ------------------------------------------------------------------
;;; Bit-vectors (GAME-STATE/DUNGEON-LEVEL-SNAPSHOT's own EXPLORED)

(defun serialize-bit-vector (bv)
  "Return BV (a bit-vector, e.g. GAME-STATE's own EXPLORED fog-of-war
mask) as a plain list of 0/1 integers, suitable for
DESERIALIZE-BIT-VECTOR to reconstruct an EQUAL bit-vector from."
  (coerce bv 'list))

(defun deserialize-bit-vector (data)
  "Inverse of SERIALIZE-BIT-VECTOR: return a fresh BIT-vector
reconstructed from DATA (a list of 0/1 integers in
SERIALIZE-BIT-VECTOR's own shape)."
  (make-array (length data) :element-type 'bit :initial-contents data))

;;; ------------------------------------------------------------------
;;; DUNGEON-LEVEL-SNAPSHOT / GAME-STATE's LEVELS FSET:MAP

(defun serialize-dungeon-level-snapshot (snapshot)
  "Return an alist capturing SNAPSHOT's MAP/ENTITIES/EXPLORED, suitable
for DESERIALIZE-DUNGEON-LEVEL-SNAPSHOT to reconstruct an equivalent
DUNGEON-LEVEL-SNAPSHOT from. ENTITIES is only included when non-NIL
(see OPT-ENTRY) -- a left level with no monsters/ground items
remaining on it (or one the player simply never triggered a spawn on)
has an empty ENTITIES list here just as often as an ENTITY has an
empty ACTIVE-EFFECTS/INVENTORY (see SERIALIZE-ENTITY's own docstring
for why an empty list needs this treatment at all)."
  (append
   (list (cons :map (serialize-game-map (dungeon-level-snapshot-map snapshot)))
         (cons :explored (serialize-bit-vector (dungeon-level-snapshot-explored snapshot))))
   (opt-entry :entities (mapcar #'serialize-entity (dungeon-level-snapshot-entities snapshot)))))

(defun deserialize-dungeon-level-snapshot (data)
  "Inverse of SERIALIZE-DUNGEON-LEVEL-SNAPSHOT: return a fresh
DUNGEON-LEVEL-SNAPSHOT reconstructed from DATA (an alist in
SERIALIZE-DUNGEON-LEVEL-SNAPSHOT's own shape)."
  (make-dungeon-level-snapshot
   :map (deserialize-game-map (cdr (assoc :map data)))
   :entities (mapcar #'deserialize-entity (cdr (assoc :entities data)))
   :explored (deserialize-bit-vector (cdr (assoc :explored data)))))

(defun serialize-levels (levels)
  "Return a vector of (DEPTH SERIALIZE-DUNGEON-LEVEL-SNAPSHOT-result)
two-element lists for LEVELS (a GAME-STATE's own LEVELS FSET:MAP, keyed
by integer depth), suitable for DESERIALIZE-LEVELS to reconstruct an
equivalent FSET:MAP from. Deliberately a VECTOR of two-element lists --
not a CONS-pair alist keyed by DEPTH -- for two reasons neither of
which is visible from a plain Lisp-to-Lisp round trip (only from a real
CL-JSON ENCODE-JSON-ALIST-TO-STRING/DECODE-JSON-FROM-STRING trip, which
PACK-SAVE-STATE/UNPACK-SAVE-STATE actually perform): first, CL-JSON's
alist encoder treats any DEPTH-keyed CONS pair as a JSON *object*
member and stringifies/re-interns DEPTH as a keyword symbol on decode
(e.g. integer 2 becomes the keyword :|2|), silently corrupting
FSET:LOOKUP's integer keys; second, a bare Lisp list value (as opposed
to a VECTOR) is ambiguous with JSON's boolean/null in CL-JSON's
encoder when empty (a fresh game that has never yet used a staircase
has an empty LEVELS map), round-tripping as the literal string \"nil\"
rather than an empty JSON array. Representing each entry as its own
two-element list, all wrapped in a VECTOR, sidesteps both problems: a
VECTOR always encodes as an unambiguous JSON array (even when empty),
and DEPTH survives untouched as a JSON number/Lisp integer inside that
array rather than becoming an object key CL-JSON would otherwise
mangle."
  (let (pairs)
    (fset:do-map (depth snapshot levels)
      (push (list depth (serialize-dungeon-level-snapshot snapshot)) pairs))
    (coerce (nreverse pairs) 'vector)))

(defun deserialize-levels (data)
  "Inverse of SERIALIZE-LEVELS: return a fresh FSET:MAP reconstructed
from DATA (a sequence of two-element (DEPTH SNAPSHOT-DATA) lists in
SERIALIZE-LEVELS's own shape). DATA is COERCEd to a LIST first since
FOLD-LEFT (the \"fold\" library function, not this file's own code)
only accepts a list -- DATA may already be one (a real CL-JSON decode
of a JSON array always produces a list, regardless of what shape
SERIALIZE-LEVELS's own VECTOR encoded from), but a caller that skips
JSON entirely and hands SERIALIZE-LEVELS's raw return value straight
to this function (as the direct, non-JSON Lisp-to-Lisp unit tests do)
would otherwise hand FOLD-LEFT a VECTOR."
  (fold-left (lambda (m pair)
               (fset:with m (first pair) (deserialize-dungeon-level-snapshot (second pair))))
             (fset:empty-map) (coerce data 'list)))

;;; ------------------------------------------------------------------
;;; GAME-STATE's FLAGS FSET:MAP
;;;
;;; FLAGS values are documented (see GAME-STATE's own FLAGS slot
;;; docstring, ARCHITECTURE_PLAN.md §9) as "arbitrary immutable value"
;;; -- today the only value ever stored is a plain keyword
;;; (:GAME-OVER-REASON's own value), so SERIALIZE-FLAGS/
;;; DESERIALIZE-FLAGS pass values through unchanged rather than
;;; recursing into any nested-CLOS-instance handling the way LEVELS'
;;; own DUNGEON-LEVEL-SNAPSHOT values need: a future flag whose value
;;; is itself a CLOS instance (rather than a keyword/number/string/
;;; plain list) would need its own SERIALIZE-FLAGS special case added
;;; here first.

(defun serialize-flags (flags)
  "Return an alist of (KEY . VALUE) pairs for FLAGS (a GAME-STATE's own
FLAGS FSET:MAP), suitable for DESERIALIZE-FLAGS to reconstruct an
equivalent FSET:MAP from."
  (let (pairs)
    (fset:do-map (key value flags)
      (push (cons key value) pairs))
    (nreverse pairs)))

(defun deserialize-flags (data)
  "Inverse of SERIALIZE-FLAGS: return a fresh FSET:MAP reconstructed
from DATA (an alist in SERIALIZE-FLAGS's own shape)."
  (fold-left (lambda (m pair) (fset:with m (car pair) (cdr pair)))
             (fset:empty-map) data))

;;; ------------------------------------------------------------------
;;; MESSAGE-LOG (a list of LOG-MESSAGE instances or bare strings)

(defun serialize-log-entry (entry)
  "Return an alist capturing ENTRY's TEXT/COLOR (read via
LOG-ENTRY-TEXT/LOG-ENTRY-COLOR, which already handle both LOG-MESSAGE
instances and bare strings -- see their own docstrings), suitable for
DESERIALIZE-LOG-ENTRY to reconstruct an equivalent LOG-MESSAGE from."
  (list (cons :text (log-entry-text entry))
        (cons :color (log-entry-color entry))))

(defun deserialize-log-entry (data)
  "Inverse of SERIALIZE-LOG-ENTRY: return a fresh LOG-MESSAGE
reconstructed from DATA (an alist in SERIALIZE-LOG-ENTRY's own shape).
Always reconstructs a LOG-MESSAGE, even for a plain-string entry that
originally predated LOG-MESSAGE -- LOG-ENTRY-TEXT/LOG-ENTRY-COLOR
already treat the two interchangeably (see their own docstrings), so
this loses nothing a caller could observe."
  (make-log-message (cdr (assoc :text data)) (cdr (assoc :color data))))

;;; ------------------------------------------------------------------
;;; GAME-STATE itself

(defun serialize-game-state (state)
  "Return an alist capturing every one of STATE's slots (PLAYER,
ENTITIES, MAP, CURRENT-DEPTH, LEVELS, EXPLORED, FLAGS, MESSAGE-LOG) --
ARCHITECTURE_PLAN.md §10's pure serialization step -- tagged with
*RDESCENT-SAVE-FORMAT-VERSION* under :SAVE-FORMAT-VERSION so
DESERIALIZE-GAME-STATE can detect a stale/incompatible blob before
attempting to reconstruct anything from it (see
*RDESCENT-SAVE-FORMAT-VERSION*'s own docstring). Every nested CLOS
instance (PLAYER/ENTITIES' own ENTITY subclasses, their INVENTORY/
EQUIPMENT/ACTIVE-EFFECTS contents, MAP's TILE array, LEVELS' own
DUNGEON-LEVEL-SNAPSHOTs, MESSAGE-LOG's LOG-MESSAGE entries) is
recursed into via this file's own SERIALIZE-* helpers, so the result
is entirely plain data (alists, lists, keywords, integers, strings) --
no CLOS instance, FSET:MAP, or bit-vector survives into the returned
value -- deliberately shaped so a future caller can safely
CL-JSON:ENCODE-JSON-ALIST-TO-STRING it (and DESERIALIZE-GAME-STATE
its CL-JSON:DECODE-JSON-FROM-STRING inverse) without this function
itself needing to change, exactly as ARCHITECTURE_PLAN.md's own future
§19 (Save/Restore via Signed Client-Side State) describes building a
thin sign/verify wrapper around this pair rather than any further
engine change. Pure: does not touch STATE itself, nor any object
reachable from it. ENTITIES/MESSAGE-LOG/FLAGS are only included when
non-NIL (see OPT-ENTRY) -- a state with no other ENTITIES on the
player's own LEVEL, an empty MESSAGE-LOG (a save made the instant a
fresh game starts, before any log line has been added), or an empty
FLAGS map (before :GAME-OVER-REASON/:DEAD-TICKS is ever set) are all
common, ordinary cases, and a bare Lisp NIL is otherwise ambiguous with
JSON false/null to CL-JSON's encoder -- it would round-trip as the
literal string \"nil\" instead of an absent key, exactly the same
hazard SERIALIZE-ENTITY's own ACTIVE-EFFECTS/INVENTORY/EQUIPMENT guard
against for the same reason (see its docstring). LEVELS does not need
the same OPT-ENTRY treatment -- SERIALIZE-LEVELS already returns an
unambiguous (possibly empty) VECTOR rather than a bare list, for a
second, LEVELS-specific reason its own docstring explains."
  (append
   (list (cons :save-format-version *rdescent-save-format-version*)
         (cons :player (serialize-entity (get-player state)))
         (cons :map (serialize-game-map (get-map state)))
         (cons :current-depth (get-current-depth state))
         (cons :levels (serialize-levels (get-levels state)))
         (cons :explored (serialize-bit-vector (get-explored state))))
   (opt-entry :entities (mapcar #'serialize-entity (get-entities state)))
   (opt-entry :message-log (mapcar #'serialize-log-entry (get-message-log state)))
   (opt-entry :flags (serialize-flags (get-flags state)))))

(defun deserialize-game-state (data)
  "Inverse of SERIALIZE-GAME-STATE: return a fresh GAME-STATE
reconstructed from DATA (an alist in SERIALIZE-GAME-STATE's own
shape). Signals an error via CHECK-SAVE-FORMAT-VERSION if DATA's own
:SAVE-FORMAT-VERSION does not match *RDESCENT-SAVE-FORMAT-VERSION*,
before attempting to reconstruct anything else from DATA -- see
*RDESCENT-SAVE-FORMAT-VERSION*'s own docstring for why this check
exists and when it should (and should not) be satisfied by bumping the
version number instead of trying to keep old blobs readable forever."
  (check-save-format-version (cdr (assoc :save-format-version data)))
  (make-instance 'game-state
                 :player (deserialize-entity (cdr (assoc :player data)))
                 :entities (mapcar #'deserialize-entity (cdr (assoc :entities data)))
                 :map (deserialize-game-map (cdr (assoc :map data)))
                 :current-depth (cdr (assoc :current-depth data))
                 :levels (deserialize-levels (cdr (assoc :levels data)))
                 :explored (deserialize-bit-vector (cdr (assoc :explored data)))
                 :flags (deserialize-flags (cdr (assoc :flags data)))
                 :message-log (mapcar #'deserialize-log-entry (cdr (assoc :message-log data)))))

(defun check-save-format-version (version)
  "Signal a SIMPLE-ERROR unless VERSION is EQL to
*RDESCENT-SAVE-FORMAT-VERSION* -- DESERIALIZE-GAME-STATE's own guard
against attempting to reconstruct a GAME-STATE from a blob produced by
an incompatible (older or newer) version of this file's serialization
format, called before any other part of DESERIALIZE-GAME-STATE runs."
  (unless (eql version *rdescent-save-format-version*)
    (error "DESERIALIZE-GAME-STATE: unsupported save-format-version ~S (expected ~S)"
           version *rdescent-save-format-version*)))

(defvar *save-state-hmac-key* (ironclad:ascii-string-to-byte-array 
                               (or (uiop:getenv "RDESCENT_SAVE_SECRET")
                                   "LOCAL_DEV_KEY_CHANGE_ME_IN_PROD"))
  "The secret key used to HMAC-SHA256 sign serialized save states.")

(defun sign-payload (compressed-bytes)
  "Appends an HMAC-SHA256 signature to the front of COMPRESSED-BYTES to prevent client tampering."
  (let ((hmac (ironclad:make-hmac *save-state-hmac-key* :sha256)))
    (ironclad:update-hmac hmac compressed-bytes)
    (let* ((digest (ironclad:hmac-digest hmac))
           (out (make-array (+ (length digest) (length compressed-bytes))
                            :element-type '(unsigned-byte 8))))
      (replace out digest)
      (replace out compressed-bytes :start1 (length digest))
      out)))

(defun verify-and-extract-payload (signed-bytes)
  "Verifies the HMAC-SHA256 signature on SIGNED-BYTES. Errors if tampering is detected, otherwise returns the un-signed COMPRESSED-BYTES."
  (let* ((digest-len 32)
         (len (length signed-bytes)))
    (when (< len digest-len) (error "Payload too small to contain signature."))
    (let ((provided-digest (subseq signed-bytes 0 digest-len))
          (compressed-bytes (subseq signed-bytes digest-len)))
      (let ((hmac (ironclad:make-hmac *save-state-hmac-key* :sha256)))
        (ironclad:update-hmac hmac compressed-bytes)
        (unless (equalp provided-digest (ironclad:hmac-digest hmac))
          (error "Save file signature mismatch. Nice try, script kiddie."))
        compressed-bytes))))

(defun pack-save-state (state)
  "Takes a GAME-STATE, serializes it to an alist, JSON-encodes it,
   compresses it with zlib, signs it, and spits out a base64 string for the client to hoard."
  (cl-base64:usb8-array-to-base64-string
   (sign-payload
    (salza2:compress-data
     (babel:string-to-octets
      (cl-json:encode-json-alist-to-string
       (serialize-game-state state))
      :encoding :utf-8)
     'salza2:zlib-compressor))))

(defun unpack-save-state (base64-string)
  "Takes a base64 string from the client, decodes it, verifies the signature,
   decompresses it, JSON-decodes it, and deserializes it back into a fresh GAME-STATE."
  (deserialize-game-state
   (cl-json:decode-json-from-string
    (babel:octets-to-string
     (chipz:decompress
      nil 'chipz:zlib
      (verify-and-extract-payload
       (cl-base64:base64-string-to-usb8-array base64-string)))
     :encoding :utf-8))))

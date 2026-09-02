;;; -*- Lisp -*-

;;; FUTURE_PLANS.md §13's Equipment & Armory system: the DEFINE-
;;; ARMORY-EQUIPPABLE-ITEM macro (which defines a stateless
;;; EQUIPPABLE-ITEM subclass and its matching MAKE-* factory from a
;;; handful of leaf data -- display name/equip slot/stat bonuses/etc)
;;; and every ordinary armory EQUIPPABLE-ITEM built from it (weapons/
;;; body armor/headgear/off-hand items). See RDESCENT/ITEMS/BASE.LISP
;;; for EQUIPPABLE-ITEM itself and this subdirectory's own file map;
;;; RDESCENT/ITEMS/LOOT.LISP's ten Rare/Legendary drops reuse this same
;;; DEFINE-ARMORY-EQUIPPABLE-ITEM macro, so it must load before that
;;; file.

(in-package "JRM-CODE-PROJECT")

(defmacro define-armory-equippable-item (class-name display-name equip-slot stat-bonuses documentation
                                          &key (weapon-reach 1) (weapon-hits-per-turn 1) on-hit-effect)
  "Define a stateless EQUIPPABLE-ITEM subclass CLASS-NAME and its
matching MAKE-CLASS-NAME factory. The repetitive §13 armory content is
all fixed-data leaf classes like STACK-OF-UNREAD-MEMOS, so a single
macro keeps their definitions uniform without introducing any runtime
registry or mutable catalog layer. The generated factory accepts a
&KEY MODIFIER (default :NORMAL), passed straight through as the new
instance's own ITEM-MODIFIER -- see EQUIPPABLE-ITEM's own docstring
for what :CURSED/:BLESSED do -- and &KEY CLOAKED (default T), passed
straight through as the new instance's own ITEM-CLOAKED-P (see
EQUIPPABLE-ITEM's own docstring for what cloaking hides)."
  (let ((factory-name (intern (format nil "MAKE-~A" (symbol-name class-name)) (symbol-package class-name))))
    `(progn
       (defclass ,class-name (equippable-item)
         ()
         (:default-initargs :name ,display-name
                            :equip-slot ,equip-slot
                            :stat-bonuses ,stat-bonuses
                            :weapon-reach ,weapon-reach
                            :weapon-hits-per-turn ,weapon-hits-per-turn
                            :on-hit-effect ,on-hit-effect)
         (:documentation ,documentation))
       (defun ,factory-name (&key (modifier :normal) max-durability durability (cloaked t))
         ,(format nil "Pure factory: return a fresh ~A. MODIFIER (default :NORMAL) is passed straight through as the new instance's own ITEM-MODIFIER. CLOAKED (default T) is passed straight through as the new instance's own ITEM-CLOAKED-P (see EQUIPPABLE-ITEM's own class docstring). MAX-DURABILITY/DURABILITY (default NIL, meaning \"use EQUIPPABLE-ITEM's own class default/derive from MAX-DURABILITY\" -- see its class docstring) are only forwarded to MAKE-INSTANCE when explicitly supplied, so a direct/test call keeps this item's usual deterministic *RDESCENT-DEFAULT-ITEM-DURABILITY*." display-name)
         (apply #'make-instance ',class-name :modifier modifier :cloaked cloaked
                (append (when max-durability (list :max-durability max-durability))
                        (when durability (list :durability durability))))))))

(defparameter *rdescent-item-modifier-weights*
  '((:normal . 76) (:cursed . 12) (:blessed . 12))
  "The weighted (KEYWORD . WEIGHT) distribution RANDOM-ITEM-MODIFIER
draws from -- the large majority (76%) of any freshly spawned
EQUIPPABLE-ITEM comes out an ordinary :NORMAL find, with a rarer, equal
12% chance each of :CURSED or :BLESSED, matching this task's own
\"most equipable items come in normal, cursed, or blessed variants\"
framing without over-favoring either extreme relative to the other.")

(defun random-item-modifier ()
  "Return a randomly chosen :NORMAL/:CURSED/:BLESSED keyword, weighted
by *RDESCENT-ITEM-MODIFIER-WEIGHTS*. Used only at the actual moment an
EQUIPPABLE-ITEM is spawned into the game world (see
RANDOMIZE-EQUIPPABLE-ITEM-MODIFIER and its call sites in PURCHASE-ITEM/
SPAWN-ITEMS-FOR-LEVEL) -- never inside any MAKE-* factory itself, whose
own default (:NORMAL, unless the caller passes an explicit :MODIFIER)
must stay deterministic for direct callers (including this engine's own
test suite) to keep relying on."
  (let ((roll (random (reduce #'+ *rdescent-item-modifier-weights* :key #'cdr))))
    (dolist (entry *rdescent-item-modifier-weights*)
      (if (< roll (cdr entry))
          (return-from random-item-modifier (car entry))
          (decf roll (cdr entry))))))

(defun item-with-modifier (item modifier)
  "Return a fresh copy of ITEM (an EQUIPPABLE-ITEM) that is identical in
every other respect but carries MODIFIER instead of ITEM's own --
constructed via COPY-EQUIPPABLE-ITEM, which preserves ITEM's own
DURABILITY/MAX-DURABILITY (as well as every fixed, class-level
:DEFAULT-INITARGS slot -- EQUIP-SLOT/STAT-BONUSES/WEAPON-REACH/WEAPON-
HITS-PER-TURN/ON-HIT-EFFECT/NAME -- MAKE-INSTANCE supplies
automatically, see DEFINE-ARMORY-EQUIPPABLE-ITEM) rather than resetting
them to the class's own defaults."
  (copy-equippable-item item :modifier modifier))

(defun item-with-durability (item durability)
  "Return a fresh copy of ITEM (an EQUIPPABLE-ITEM) that is identical in
every other respect but carries DURABILITY instead of ITEM's own
current GET-DURABILITY -- MAX-DURABILITY and every other slot
(including MODIFIER) are preserved via COPY-EQUIPPABLE-ITEM. Used by
APPLY-EQUIPMENT-WEAR to record a reduced durability after a connecting
hit; see EQUIPPABLE-ITEM's own class docstring."
  (copy-equippable-item item :durability durability))

(defun item-uncloaked (item)
  "Return a fresh copy of ITEM (an EQUIPPABLE-ITEM) that is identical
in every other respect but carries CLOAKED NIL -- i.e. permanently
reveals ITEM's own \"Cursed \"/\"Blessed \" MODIFIER prefix in
GET-ITEM-NAME from now on, regardless of ITEM's own current
ITEM-CLOAKED-P. Called by EQUIP-ITEM (RDESCENT/ACTIONS.LISP) the
moment an item is actually equipped -- see EQUIPPABLE-ITEM's own class
docstring for why cloaking exists at all. Idempotent: calling this on
an already-uncloaked item just returns another uncloaked copy."
  (copy-equippable-item item :cloaked nil))

(defun copy-equippable-item (item &key (modifier (item-modifier item))
                                        (max-durability (get-max-durability item))
                                        (durability (get-durability item))
                                        (cloaked (item-cloaked-p item)))
  "Return a fresh copy of ITEM (an EQUIPPABLE-ITEM), an instance of
ITEM's own concrete CLASS-OF, carrying MODIFIER/MAX-DURABILITY/
DURABILITY/CLOAKED (each defaulting to ITEM's own current value, so
any subset can be overridden while every other per-instance slot is
preserved verbatim) -- every remaining slot (EQUIP-SLOT/STAT-BONUSES/
WEAPON-REACH/WEAPON-HITS-PER-TURN/ON-HIT-EFFECT/NAME) is a fixed,
class-level :DEFAULT-INITARGS value MAKE-INSTANCE supplies
automatically (see DEFINE-ARMORY-EQUIPPABLE-ITEM). This is the shared
helper behind ITEM-WITH-MODIFIER, ITEM-WITH-DURABILITY, and
ITEM-UNCLOAKED, so none of the three ever has to worry about
accidentally resetting either of the others' own state."
  (make-instance (class-of item)
                 :modifier modifier
                 :max-durability max-durability
                 :durability durability
                 :cloaked cloaked))

(defmethod initialize-instance :after ((item equippable-item) &key durability &allow-other-keys)
  "Default DURABILITY to this ITEM's own MAX-DURABILITY when the
:DURABILITY initarg wasn't explicitly supplied (or was explicitly NIL)
-- see EQUIPPABLE-ITEM's own class docstring for why a freshly
constructed item always starts at full durability unless a caller
(e.g. DESERIALIZE-ITEM, restoring a previously worn item) says
otherwise."
  (declare (ignore durability))
  (unless (slot-value item 'durability)
    (setf (slot-value item 'durability) (get-max-durability item))))

(defun randomize-newly-spawned-equippable-item (item)
  "Return a fresh copy of ITEM with a randomly rolled ITEM-MODIFIER (see
RANDOM-ITEM-MODIFIER/ITEM-WITH-MODIFIER) and a randomly rolled, full
MAX-DURABILITY/DURABILITY (uniformly between *RDESCENT-ITEM-DURABILITY-
MIN* and *RDESCENT-ITEM-DURABILITY-MAX*, inclusive) if ITEM is an
EQUIPPABLE-ITEM, otherwise ITEM itself unchanged (a scroll, bomb, or
other non-equippable RDESCENT-ITEM has no notion of MODIFIER or
DURABILITY worth rolling). This is the single call this engine makes at
each real point an equippable item is actually placed into the game
world -- PURCHASE-ITEM (RDESCENT/ACTIONS.LISP) and SPAWN-ITEMS-FOR-
LEVEL (RDESCENT/DUNGEON.LISP) -- so that every other MAKE-*/MAKE-
GROUND-* factory (including the ones this same call ultimately goes
through) keeps its own deterministic :NORMAL modifier and
*RDESCENT-DEFAULT-ITEM-DURABILITY* default for direct callers, most
importantly this engine's own test suite."
  (if (typep item 'equippable-item)
      (let ((rolled-max (+ *rdescent-item-durability-min*
                            (random (1+ (- *rdescent-item-durability-max*
                                            *rdescent-item-durability-min*))))))
        (copy-equippable-item item
                              :modifier (random-item-modifier)
                              :max-durability rolled-max
                              :durability rolled-max))
      item))

;; Retained as an alias: earlier revisions of this feature rolled only
;; MODIFIER at spawn time under this name -- kept so any external/older
;; caller referring to it by its original name still works.
(setf (symbol-function 'randomize-equippable-item-modifier)
      #'randomize-newly-spawned-equippable-item)

;;; FUTURE_PLANS.md §13: Equipment System & Armory

(define-armory-equippable-item keyboard-of-kinesis
  "Keyboard of Kinesis (Epic)"
  :weapon
  (list :power 8 :pivot 2)
  "Epic ergonomic main-hand weapon. Its real mechanics are a strong
+8 :POWER bonus, +2 :PIVOT, and a 35% on-hit :CARPAL-TUNNEL debuff
that slows the target's attack cadence by doubling attack ENERGY cost.
The plan's \"two-handed (no shield/off-hand)\" rider is deliberately
not enforced: the equipment system only has one independent :WEAPON
slot plus a generic :OFF-HAND slot, with no existing primitive for a
weapon occupying both at once."
  :on-hit-effect (list :kind :carpal-tunnel :turns *rdescent-carpal-tunnel-ticks* :chance 0.35))

(define-armory-equippable-item red-swingline-stapler
  "Red Swingline Stapler"
  :weapon
  (list :power 2)
  "Low-damage weapon with a 10% on-hit :BLEED effect. :BLEED is wired
for real as a small damage-over-time effect via STATUS-EFFECT's own
MAGNITUDE slot; the plan text's additional \"panic the target\" rider
is deliberately deferred because the current AI has no temporary panic
status that can cleanly override disposition/pathing."
  :on-hit-effect (list :kind :bleed :turns *rdescent-bleed-ticks*
                       :magnitude *rdescent-bleed-damage-per-tick* :chance 0.10))

(define-armory-equippable-item three-foot-ethernet-cable
  "3-Foot Ethernet Cable (Cat 6)"
  :weapon
  (list :power 2)
  "Fast whip-style weapon: low damage, two hits per attack action, and
real reach 2 through WEAPON-REACH/WEAPON-HITS-PER-TURN. The plan text's
crowd-control flavor is therefore approximated through the existing
combat scheduler rather than a new knockback or entangling subsystem."
  :weapon-reach 2
  :weapon-hits-per-turn 2)

(define-armory-equippable-item severed-server-rack-rail
  "Severed Server Rack Rail"
  :weapon
  (list :power 11)
  "Heavy blunt weapon with very high +11 :POWER. Its promised movement-
speed penalty is deliberately omitted because RDESCENT currently has no
separate movement-speed subsystem beyond the shared ENERGY scheduler."
  )

(define-armory-equippable-item razor-sharp-aluminum-mousepad
  "Razor-Sharp Aluminum Mousepad"
  :weapon
  (list :power 2 :pivot 4)
  "Dagger-style weapon whose planned critical-hit identity is
approximated as a substantial +4 :PIVOT bonus (feeding directly into
EFFECTIVE-DODGE-CHANCE) plus light damage, since the engine has no
separate critical-hit chance system."
  )

(define-armory-equippable-item telescoping-pointer
  "Telescoping Pointer (Laser Inactive)"
  :weapon
  (list :power 4)
  "Spear-style weapon with real reach 2. The plan text's additional
\"pierces through the first enemy to hit the one behind it\" mechanic is
deliberately deferred because bump-combat has no existing multi-target-
in-a-line resolution primitive."
  :weapon-reach 2)

(define-armory-equippable-item whiteboard-marker-of-dominance
  "Whiteboard Marker of Dominance"
  :weapon
  (list :power 8)
  "High-damage weapon implemented as a plain +8 :POWER bonus. Its
durability/ink depletion and \"bypasses Project Manager armor\" riders
are deliberately omitted because the engine has neither consumable
durability tracking nor attack damage typing/armor-tag exceptions."
  )

(define-armory-equippable-item mechanical-keyboard
  "Mechanical Keyboard (Cherry MX Blue)"
  :weapon
  (list :power 6)
  "Loud sonic-flavored weapon implemented as a single-target +6 :POWER
main hand with an on-hit :DISTRACTED effect. The planned 1-tile-radius
AoE slam is deliberately omitted because weapon attacks still resolve
against only one target at a time."
  :on-hit-effect (list :kind :distracted :turns *rdescent-distraction-ticks*))

(define-armory-equippable-item rubber-band-gatling-gun
  "Rubber Band Gatling Gun"
  :weapon
  (list :power 1)
  "Rapid-fire ranged weapon: real reach 4 and three hits per attack
action. Ammo is deliberately not tracked; the gun fires unconditionally
every turn because RDESCENT has no inventory-of-ammo or reload system."
  :weapon-reach 4
  :weapon-hits-per-turn 3)

(define-armory-equippable-item nerf-retaliator
  "Nerf Retaliator (Office Modded)"
  :weapon
  (list :power 10)
  "High-damage ranged weapon with real reach 4. Its foam-dart ammo and
full-turn reload are deliberately omitted because the current combat
model has no reload state or per-weapon alternating ready/unready
cycle."
  :weapon-reach 4)

(define-armory-equippable-item can-of-compressed-air
  "Can of Compressed Air"
  :weapon
  (list :power 4)
  "Single-target ranged blaster with real reach 3 and an on-hit
:STUNNED effect that causes the next eligible turn to be skipped. The
planned 3-tile cone AoE and pushback are deliberately omitted because
the engine has no multi-target cone or forced-movement primitive."
  :weapon-reach 3
  :on-hit-effect (list :kind :stunned :turns *rdescent-stunned-ticks*))

(define-armory-equippable-item usb-drive-shuriken
  "USB Drive Shuriken"
  :weapon
  (list :power 6)
  "Thrown ranged weapon with real reach 4 and solid +6 :POWER. Its
retrieval mechanic is deliberately omitted because equipped weapons are
not temporarily removed from the wielder or embedded in corpses."
  :weapon-reach 4)

(define-armory-equippable-item megaphone-of-lets-take-this-offline
  "Megaphone of \"Let's Take This Offline\""
  :weapon
  (list :power 7)
  "Psychic/flavor weapon implemented as a strong +7 :POWER main hand
with an on-hit :ANALYSIS-PARALYSIS debuff. Its planned localized
silence field, special-attack lockout, Energy-per-shot resource cost,
and two-handed restriction are all deliberately omitted because none of
those subsystems currently exist as separate combat primitives."
  :weapon-reach 3
  :on-hit-effect (list :kind :analysis-paralysis :turns *rdescent-analysis-paralysis-ticks*))

(define-armory-equippable-item laser-pointer-of-redirection
  "Laser Pointer of Redirection"
  :weapon
  (list :pivot 2)
  "Utility wand implemented as a zero-damage weapon with reach 4 and a
small +2 :PIVOT bonus. The plan text's forced-direction movement effect
is deliberately deferred because enemy AI has no \"on next turn, walk in
the pointed direction\" primitive to plug into."
  :weapon-reach 4)

(define-armory-equippable-item reply-all-blunderbuss
  "\"Reply-All\" Blunderbuss"
  :weapon
  (list :power 10)
  "Psychic shotgun approximated as a high-damage reach-3 weapon. Its
massive cone attack and explicit 20-Energy-per-shot rider are
deliberately omitted because weapon attacks still target only one
entity and the current ENERGY system models action timing, not a second
spell-point resource."
  :weapon-reach 3)

(define-armory-equippable-item hr-whistleblower
  "HR Whistleblower"
  :weapon
  (list :power 6 :defense 3)
  "Artifact-tier weapon implemented as a strong passive stat stick:
+6 :POWER and +3 :DEFENSE. The planned \"summon an invulnerable HR Rep
NPC that hunts Middle Managers\" mechanic is deliberately deferred
because RDESCENT has no summon-follow-AI or one-floor charge primitive
for equipment activations."
  :weapon-reach 4)

(define-armory-equippable-item startup-green-t-shirt
  "Startup Green T-Shirt"
  :body
  (list :max-hp 1)
  "Light body armor granting +1 effective MAX-HP. The plan text's
\"Code Monkeys treat the wearer as friendly for the first 3 turns\"
behavior is deliberately deferred because current monster disposition is
chosen at spawn time, not re-evaluated from temporary turn counters."
  )

(define-armory-equippable-item patagonia-fleece-vest
  "Patagonia Fleece Vest"
  :body
  (list :defense 3 :synergy 2)
  "Medium armor: +3 :DEFENSE plus +2 :SYNERGY, hooking directly into
the already-existing vendor-price discount and spawn-time pacify-chance
formulas."
  )

(define-armory-equippable-item unwashed-hoodie
  "Unwashed Hoodie"
  :body
  (list :defense 4 :hygiene -3)
  "Heavy armor: +4 :DEFENSE and a real permanent-while-equipped -3
:HYGIENE penalty, which feeds straight into future-level spawn-time
faction hostility through EFFECTIVE-HYGIENE. Like every other
EQUIPPABLE-ITEM, an individual find of one may randomly turn out
:CURSED (UNEQUIP-ITEM then permanently refuses to remove it -- see
EQUIPPABLE-ITEM's own docstring) or :BLESSED rather than :NORMAL.")

(define-armory-equippable-item ironed-button-down
  "Ironed Button-Down"
  :body
  (list :defense 2)
  "Suit-style armor implemented as a modest +2 :DEFENSE bonus. Its
planned heightened resistance specifically to PIP psychic attacks is
deliberately approximated this way because the combat engine has no
attack damage-type tagging beyond the separate consumable-item code
paths."
  )

(define-armory-equippable-item headphones-of-noise-canceling
  "Headphones of Noise-Canceling"
  :head
  nil
  "Head-slot accessory whose real mechanical effect is complete
immunity to :CONFUSED in APPLY-STATUS-EFFECT, sharing the same
deflection seam Buzzword Immunity already uses. With only one :HEAD
slot in the equipment system, this mutually excludes other head/neck
accessories instead of stacking."
  )

(define-armory-equippable-item lanyard-of-the-vip
  "Lanyard of the VIP"
  :head
  (list :seniority 2)
  "Neck-flavored accessory implemented in the shared :HEAD slot, with a
real +2 :SENIORITY bonus feeding deflection/detection formulas. Its
planned \"SecOps Auditor aggro radius to zero\" behavior is deliberately
deferred because that monster/archetype-specific aggro mechanic does
not exist yet."
  )

(define-armory-equippable-item blue-light-blocking-glasses
  "Blue-Light Blocking Glasses"
  :head
  (list :domain-knowledge 2)
  "Head-slot accessory granting a real +2 :DOMAIN-KNOWLEDGE bonus plus
a separate flat +1 FOV radius via EFFECTIVE-FOV-RADIUS, exactly as the
plan text specifies."
  )

(define-armory-equippable-item yubikey-of-second-factors
  "YubiKey of Second Factors"
  :head
  nil
  "Amulet-style accessory implemented in the shared :HEAD slot. Its
flagship once-per-floor death-save mechanic is wired for real in the
player-damage paths: a lethal hit shatters the YubiKey, leaves the
wearer at 1 HP, and teleports them to a random safe tile on the current
floor."
  )

(define-armory-equippable-item aws-certified-solutions-architect-plaque
  "AWS Certified Solutions Architect Plaque"
  :off-hand
  (list :defense 3 :domain-knowledge 1)
  "Shield-style off-hand granting +3 :DEFENSE and +1
:DOMAIN-KNOWLEDGE. Its planned elemental cloud/database damage
resistance is deliberately deferred because the combat engine has no
elemental damage typing to selectively halve."
  )

(define-armory-equippable-item agile-scrum-master-certificate
  "Agile Scrum Master Certificate"
  :off-hand
  (list :synergy 4)
  "Off-hand certificate granting a real +4 :SYNERGY bonus. The plan
text's terror/flee aura for Code Monkeys is deliberately deferred
because current AI has no equipment-driven fear override or temporary
disposition flip-on-sight mechanic."
  )

(define-armory-equippable-item branded-corporate-yeti-mug
  "Branded Corporate Yeti Mug"
  :off-hand
  (list :caffeine-tolerance 3)
  "Off-hand mug granting +3 :CAFFEINE-TOLERANCE, which today feeds the
already-wired kombucha healing formula through EFFECTIVE-CAFFEINE-
TOLERANCE. Its planned refill/drain-rate behavior is deliberately
deferred because no passive CAFFEINE-TOLERANCE depletion system exists
yet."
  )

(define-armory-equippable-item stack-overflow-plagiarized-script
  "Stack Overflow Plagiarized Script"
  :off-hand
  (list :power 6)
  "Off-hand tome granting a large +6 :POWER bonus. The plan text's 5%
self-backfire / skip-your-own-turn risk is deliberately deferred
because player attacks currently have no independent post-hit self-
stun/backfire roll hook."
  )


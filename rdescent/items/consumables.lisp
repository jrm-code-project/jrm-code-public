;;; -*- Lisp -*-

;;; FUTURE_PLANS.md §17's "Corporate Pharmacy": ROOT-PASSWORD-POST-
;;; IT-NOTE (a bespoke Rare-tier CONSUMABLE-ITEM), the DEFINE-
;;; CONSUMABLE-ITEM macro, and the three-tier catalog of ordinary
;;; CONSUMABLE-ITEMs built from it (Tier 1 "Everyday Grub", Tier 2
;;; "The Caffeine Aisle", Tier 3 "The Hard Stuff", plus Rare Loot's own
;;; consumable), plus *RDESCENT-VENDOR-STOCK-TABLE* (FUTURE_PLANS.md
;;; §10's fixed VENDOR-FIXTURE catalog), which is defined here -- after
;;; every MAKE-* factory it #'-quotes, from this file and from
;;; RDESCENT/ITEMS/EQUIPMENT.LISP/LOOT.LISP before it -- because each
;;; entry's PAYLOAD function must already be FBOUNDP at load time (see
;;; that DEFPARAMETER's own docstring). Loaded after RDESCENT/ITEMS/
;;; EQUIPMENT.LISP and RDESCENT/ITEMS/LOOT.LISP.

(in-package "JRM-CODE-PROJECT")

(defclass root-password-post-it-note (consumable-item)
  ()
  (:default-initargs :name "Root Password Post-It Note")
  (:documentation "A Rare-tier consumable, bespoke rather than
DEFINE-CONSUMABLE-ITEM-generated (FUTURE_PLANS.md §15): using it grants
a permanent, never-consumed \"skeleton key\" (STATE's own
:SKELETON-KEY-ACTIVE flag, see KEY-HELD-P, MECHANICS.LISP) that
unlocks *every* locked door for the rest of the run, regardless of
which specific key-id that door is tagged with -- unlike any ordinary
KEY-ID picked up via GRAB-ITEM's own :KEY payload, whose ADD-KEY-HELD/
REMOVE-KEY-HELD bookkeeping is single-use per matching door. This
doesn't fit DEFINE-CONSUMABLE-ITEM's declarative shape (no HEAL-
AMOUNT/ENERGY-RESTORE/STATUS-EFFECT/STAT-OVERRIDE slot models
\"permanently flip a GAME-STATE-level flag\") so, like the Unmarked
Nootropic Stack before it, it gets its own bespoke APPLY-ITEM method
instead. See MAKE-ROOT-PASSWORD-POST-IT-NOTE."))

(defun make-root-password-post-it-note ()
  "Pure factory: return a fresh ROOT-PASSWORD-POST-IT-NOTE, a
CONSUMABLE-ITEM named \"Root Password Post-It Note\" (see its own
APPLY-ITEM method for what using one does)."
  (make-instance 'root-password-post-it-note))

(defmacro define-consumable-item (class-name display-name documentation
                                   &key heal-amount energy-restore effect cleanse-kind stat-overrides flavor-text)
  "Define a stateless CONSUMABLE-ITEM subclass CLASS-NAME and its
matching MAKE-CLASS-NAME factory -- see CONSUMABLE-ITEM's own
docstring for exactly what HEAL-AMOUNT/ENERGY-RESTORE/EFFECT/
CLEANSE-KIND/STAT-OVERRIDES/FLAVOR-TEXT each do once APPLY-ITEM
(RDESCENT/ACTIONS.LISP) consumes one. §17's Corporate Pharmacy content
is all fixed-data leaf classes, exactly like §13's armory content and
DEFINE-ARMORY-EQUIPPABLE-ITEM, so a single macro keeps their
definitions uniform without introducing any runtime registry or
mutable catalog layer. EFFECT/STAT-OVERRIDES are spliced into
:DEFAULT-INITARGS unevaluated, exactly like DEFINE-ARMORY-EQUIPPABLE-
ITEM's own ON-HIT-EFFECT -- pass a (LIST ...) form (not a bare quoted
plist) so any *RDESCENT-*-TICKS*/*-MAGNITUDE* constant referenced
inside it is looked up fresh every time MAKE-INSTANCE actually runs,
not baked in at DEFINE-CONSUMABLE-ITEM's own macroexpansion time."
  (let ((factory-name (intern (format nil "MAKE-~A" (symbol-name class-name)) (symbol-package class-name))))
    `(progn
       (defclass ,class-name (consumable-item)
         ()
         (:default-initargs :name ,display-name
                            :heal-amount ,heal-amount
                            :energy-restore ,energy-restore
                            :effect ,effect
                            :cleanse-kind ,cleanse-kind
                            :stat-overrides ,stat-overrides
                            :flavor-text ,flavor-text)
         (:documentation ,documentation))
       (defun ,factory-name ()
         ,(format nil "Pure factory: return a fresh ~A, a CONSUMABLE-ITEM named ~S (see USE-ITEM for what using one does)." class-name display-name)
         (make-instance ',class-name)))))

;;; §17 The Corporate Pharmacy, Tier 1 -- "Everyday Grub": cheap,
;;; mostly-safe, small heals, all deliberately findable/purchasable
;;; from *RDESCENT-ITEM-SPAWN-TABLE*/*RDESCENT-VENDOR-STOCK-TABLE* at
;;; the earliest depths, matching the plan text's own "cheap and
;;; commonly found" framing.

(define-consumable-item stale-croissant "Stale Croissant"
  "A Tier 1 pharmacy consumable: heals a modest 5 HP, nothing else --
the baseline, no-frills snack every other Tier 1 item is judged
against."
  :heal-amount 5
  :flavor-text "You choke down the Stale Croissant. It's better than nothing. +5 HP.")

(define-consumable-item day-old-breakroom-pizza "Day-Old Breakroom Pizza"
  "A Tier 1 pharmacy consumable: heals a solid 12 HP."
  :heal-amount 12
  :flavor-text "You wolf down a slice of Day-Old Breakroom Pizza. +12 HP.")

(define-consumable-item someone-elses-tupperware-lunch "Someone Else's Tupperware Lunch"
  "A Tier 1 pharmacy consumable: heals a generous 20 HP, but carries a
10% EFFECT chance of inflicting :FOOD-POISONING on yourself instead
(*RDESCENT-FOOD-POISONING-TICKS*, *RDESCENT-FOOD-POISONING-DAMAGE-PER-
TICK* -- a slow HP drain, exactly like §13's :BLEED) -- the classic
office-fridge gamble, matching the plan text's \"whoever ate this
first clearly didn't finish it\" framing."
  :heal-amount 20
  :effect (list :chance 10 :kind :food-poisoning :ticks-remaining *rdescent-food-poisoning-ticks*
                :magnitude *rdescent-food-poisoning-damage-per-tick*)
  :flavor-text "You raid Someone Else's Tupperware Lunch from the breakroom fridge. +20 HP. (Was that... mold?)")

(define-consumable-item happy-birthday-sheet-cake "Happy Birthday!! Sheet Cake"
  "A Tier 1 pharmacy consumable: heals to full HP outright (:FULL) --
the office birthday-cake sugar rush, matching the plan text's
\"heals you completely.\""
  :heal-amount :full
  :flavor-text "You devour a corner of the Happy Birthday!! Sheet Cake. Full HP restored!")

(define-consumable-item handful-of-free-office-almonds "Handful of Free Office Almonds"
  "A Tier 1 pharmacy consumable: heals a tiny 3 HP -- the plan text's
own \"barely worth eating, but it's free\" framing, deliberately the
weakest item in the whole catalog."
  :heal-amount 3
  :flavor-text "You grab a Handful of Free Office Almonds from the community jar. +3 HP.")

;;; §17 Tier 2 -- "The Caffeine Aisle": mostly Energy-focused, with
;;; the first EXPIRE-INTO chained buff/crash pair (Quadruple Shot
;;; Espresso).

(define-consumable-item tgif-leftover-beer "TGIF Leftover Beer"
  "A Tier 2 pharmacy consumable: restores 40 Energy, but inflicts a
temporary :BUZZED debuff on yourself (*RDESCENT-BUZZED-TICKS*, a flat
DOMAIN-KNOWLEDGE penalty via EFFECTIVE-DOMAIN-KNOWLEDGE) -- \"slightly
buzzed at your desk\" from the plan text."
  :energy-restore 40
  :effect (list :kind :buzzed :ticks-remaining *rdescent-buzzed-ticks*)
  :flavor-text "You crack open a TGIF Leftover Beer from the office fridge. +40 Energy. (You feel a little buzzed.)")

(define-consumable-item breakroom-coffee-burnt "Breakroom Coffee (Burnt)"
  "A Tier 2 pharmacy consumable: restores a modest 30 Energy, nothing
else -- the plan text's own \"tastes like a scorched pot, but it
works\" framing."
  :energy-restore 30
  :flavor-text "You choke down some Breakroom Coffee (Burnt). +30 Energy.")

(define-consumable-item artisan-latte "Artisan Latte"
  "A Tier 2 pharmacy consumable: restores a generous 60 Energy --
the plan text's own \"overpriced, but effective\" framing."
  :energy-restore 60
  :flavor-text "You savor an Artisan Latte. +60 Energy.")

(define-consumable-item quadruple-shot-espresso "Quadruple Shot Espresso"
  "A Tier 2 pharmacy consumable: restores 80 Energy and grants a
temporary :CAFFEINATED speed buff (halves EFFECTIVE-ATTACK-ENERGY-
COST -- \"attacks come faster\") that, once it wears off
(*RDESCENT-CAFFEINATED-TICKS*), chains via EXPIRE-INTO into a one-tick
:DISTRACTED \"crash\" (reusing §7's existing :DISTRACTED status-effect
kind, which already skips one energy tick) -- the plan text's own
\"you will crash, hard\" framing."
  :energy-restore 80
  :effect (list :kind :caffeinated :ticks-remaining *rdescent-caffeinated-ticks*
                :expire-into (list :kind :distracted :ticks-remaining 1))
  :flavor-text "You slam a Quadruple Shot Espresso. +80 Energy. Your hands are shaking, but you feel FAST.")

(define-consumable-item warm-monster-energy-drink "Warm Monster Energy Drink"
  "A Tier 2 pharmacy consumable: restores Energy to max outright
(:FULL, *RDESCENT-PHARMACY-FULL-ENERGY-RESTORE*) -- the plan text's
own \"lukewarm, but it does the job\" framing, the Tier 2 counterpart
to Tier 1's Happy Birthday Sheet Cake full heal."
  :energy-restore :full
  :flavor-text "You chug a Warm Monster Energy Drink. Energy fully restored!")

(define-consumable-item the-smart-water "The Smart Water"
  "A Tier 2 pharmacy consumable: heals 10 HP and cleanses an active
:FOOD-POISONING debuff outright (see Someone Else's Tupperware
Lunch) -- the plan text's own \"expensive, but it cures what ails
you\" framing."
  :heal-amount 10
  :cleanse-kind :food-poisoning
  :flavor-text "You drink The Smart Water. +10 HP. (Whatever was making you sick is gone.)")

;;; §17 Tier 3 -- "The Hard Stuff": high-risk, high-reward permanent/
;;; long-duration buffs, most gated to deeper spawn tables (see
;;; *RDESCENT-ITEM-SPAWN-TABLE*).

(define-consumable-item discarded-adderall "Discarded Adderall"
  "A Tier 3 pharmacy consumable: instantly restores Energy to max
(:FULL) and grants a long :ADDERALL-FOCUS buff (+DOMAIN-KNOWLEDGE via
EFFECTIVE-DOMAIN-KNOWLEDGE) that, once it wears off
(*RDESCENT-ADDERALL-FOCUS-TICKS*), chains via EXPIRE-INTO into a
one-tick :ADDERALL-CRASH (a flat *RDESCENT-ADDERALL-CRASH-HP-DELTA*
HP hit, applied exactly once by TICK-STATUS-EFFECTS' own per-tick
MAGNITUDE-drain mechanism before the 1-tick effect itself expires) --
the plan text's own \"lose 10 HP when it wears off\" framing."
  :energy-restore :full
  :effect (list :kind :adderall-focus :ticks-remaining *rdescent-adderall-focus-ticks*
                :expire-into (list :kind :adderall-crash :ticks-remaining 1
                                   :magnitude *rdescent-adderall-crash-hp-delta*))
  :flavor-text "You dry-swallow a Discarded Adderall you found on the floor. Energy fully restored. You feel laser-focused.")

(define-consumable-item modafinil "Modafinil"
  "A Tier 3 pharmacy consumable: grants a long :MODAFINIL-IMMUNITY
buff (*RDESCENT-MODAFINIL-TICKS*) -- while active, APPLY-STATUS-EFFECT
shrugs off any incoming :CONFUSED/:STUNNED infliction outright,
exactly like the Outdated Buzzword Bingo Chips/Headphones of
Noise-Canceling collectible-set immunities -- the plan text's own
\"total immunity to sleep/stun mechanics ... you do not blink, you do
not yawn\" framing."
  :effect (list :kind :modafinil-immunity :ticks-remaining *rdescent-modafinil-ticks*)
  :flavor-text "You take a Modafinil. Your eyes go wide. You will not be sleeping for a while.")

(define-consumable-item dexedrine-spansule "Dexedrine Spansule"
  "A Tier 3 pharmacy consumable: grants a temporary :THE-ZONE buff
(*RDESCENT-THE-ZONE-TICKS*, +*RDESCENT-THE-ZONE-POWER-BONUS* via
EFFECTIVE-POWER -- standing in for the plan text's own \"guaranteed
critical hits,\" since no independent crit-multiplier subsystem exists
to hook a guaranteed-crit flag into), and permanently zeroes your own
HYGIENE via STAT-OVERRIDES -- the plan text's own \"you stop showering
... this is permanent\" framing."
  :effect (list :kind :the-zone :ticks-remaining *rdescent-the-zone-ticks*)
  :stat-overrides (list :hygiene 0)
  :flavor-text "You swallow a Dexedrine Spansule whole. You are UNSTOPPABLE. (You also stop showering. This is permanent.)")

(define-consumable-item baggie-of-blow-executive-grade "Baggie of Blow (Executive Grade)"
  "A Tier 3 pharmacy consumable: heals to full HP and restores Energy
to max outright (both :FULL), and grants a short :EXECUTIVE-HIGH buff
(*RDESCENT-EXECUTIVE-HIGH-TICKS*, +*RDESCENT-EXECUTIVE-HIGH-STAT-
BONUS* to both EFFECTIVE-SYNERGY/EFFECTIVE-PIVOT) that, once it wears
off, chains via EXPIRE-INTO into the much longer :COMEDOWN debuff
(*RDESCENT-COMEDOWN-TICKS*, the same two stats penalized by
*RDESCENT-COMEDOWN-STAT-PENALTY*, plus doubled EFFECTIVE-ATTACK-
ENERGY-COST standing in for the plan text's own \"halves movement
speed\") -- the plan text's own \"10 turns later, a catastrophic
Comedown\" framing."
  :heal-amount :full
  :energy-restore :full
  :effect (list :kind :executive-high :ticks-remaining *rdescent-executive-high-ticks*
                :expire-into (list :kind :comedown :ticks-remaining *rdescent-comedown-ticks*))
  :flavor-text "You do a bump of the Baggie of Blow (Executive Grade). You are a GOD among middle managers. Full HP and Energy restored!")

(define-consumable-item microdose-tab-lsd "Microdose Tab (LSD)"
  "A Tier 3 pharmacy consumable: grants a long :MICRODOSING buff
(*RDESCENT-MICRODOSING-TICKS*) that doubles EFFECTIVE-FOV-RADIUS and
guarantees hidden TRAP-FIXTURE detection (see MAYBE-REVEAL-HIDDEN-
ENTITIES) while active -- the plan text's own \"hidden doors/traps
glow neon colors\" left as flavor text rather than a new rendering
hook, since guaranteed detection already delivers the mechanically
meaningful half of the effect."
  :effect (list :kind :microdosing :ticks-remaining *rdescent-microdosing-ticks*)
  :flavor-text "You place a Microdose Tab (LSD) under your tongue. The cubicle walls seem... softer, somehow.")

(defclass unmarked-nootropic-stack (consumable-item)
  ()
  (:default-initargs :name "Unmarked Nootropic Stack")
  (:documentation "A Tier 3 pharmacy consumable, bespoke rather than
DEFINE-CONSUMABLE-ITEM-generated: a genuine 50/50 either/or gamble that
doesn't fit that macro's declarative \"every slot always applies\"
shape (a chance-gated EFFECT there still lets every *other* slot apply
unconditionally; this item's two outcomes are instead mutually
exclusive alternatives to *the same* roll). See its own APPLY-ITEM
method for the exact 50/50 branching: a flat coin flip either grants
the same :ADDERALL-FOCUS buff Discarded Adderall does, or slams HP
down to 1 -- the plan text's own \"50% chance of a domain-knowledge
buff, 50% chance of a very bad time\" framing. See MAKE-UNMARKED-
NOOTROPIC-STACK."))

(defun make-unmarked-nootropic-stack ()
  "Pure factory: return a fresh UNMARKED-NOOTROPIC-STACK, a
CONSUMABLE-ITEM named \"Unmarked Nootropic Stack\" (see its own
APPLY-ITEM method for what using one does)."
  (make-instance 'unmarked-nootropic-stack))

(defparameter *rdescent-vendor-stock-table*
  (list (make-vendor-stock-entry :name "Kombucha" :base-price 50 :payload :kombucha)
        (make-vendor-stock-entry :name "Scroll of PIP" :base-price 300 :payload #'make-scroll-of-pip)
        (make-vendor-stock-entry :name "Vague Re-Org Memo" :base-price 400 :payload #'make-reorg-memo)
        (make-vendor-stock-entry :name "Reply-All Bomb" :base-price 500 :payload #'make-reply-all-bomb)
        (make-vendor-stock-entry :name "Stale Croissant" :base-price 20 :payload #'make-stale-croissant)
        (make-vendor-stock-entry :name "Day-Old Breakroom Pizza" :base-price 40 :payload #'make-day-old-breakroom-pizza)
        (make-vendor-stock-entry :name "Someone Else's Tupperware Lunch" :base-price 60
                                 :payload #'make-someone-elses-tupperware-lunch)
        (make-vendor-stock-entry :name "Happy Birthday!! Sheet Cake" :base-price 150
                                 :payload #'make-happy-birthday-sheet-cake)
        (make-vendor-stock-entry :name "Handful of Free Office Almonds" :base-price 10
                                 :payload #'make-handful-of-free-office-almonds)
        (make-vendor-stock-entry :name "TGIF Leftover Beer" :base-price 80 :payload #'make-tgif-leftover-beer)
        (make-vendor-stock-entry :name "Breakroom Coffee (Burnt)" :base-price 60
                                 :payload #'make-breakroom-coffee-burnt)
        (make-vendor-stock-entry :name "Artisan Latte" :base-price 130 :payload #'make-artisan-latte)
        (make-vendor-stock-entry :name "Quadruple Shot Espresso" :base-price 170
                                 :payload #'make-quadruple-shot-espresso)
        (make-vendor-stock-entry :name "Warm Monster Energy Drink" :base-price 200
                                 :payload #'make-warm-monster-energy-drink)
        (make-vendor-stock-entry :name "The Smart Water" :base-price 220 :payload #'make-the-smart-water)
        (make-vendor-stock-entry :name "Discarded Adderall" :base-price 600 :payload #'make-discarded-adderall)
        (make-vendor-stock-entry :name "Modafinil" :base-price 700 :payload #'make-modafinil)
        (make-vendor-stock-entry :name "Dexedrine Spansule" :base-price 750 :payload #'make-dexedrine-spansule)
        (make-vendor-stock-entry :name "Baggie of Blow (Executive Grade)" :base-price 1000
                                 :payload #'make-baggie-of-blow-executive-grade)
        (make-vendor-stock-entry :name "Microdose Tab (LSD)" :base-price 850 :payload #'make-microdose-tab-lsd)
        (make-vendor-stock-entry :name "Unmarked Nootropic Stack" :base-price 500
                                 :payload #'make-unmarked-nootropic-stack)
        (make-vendor-stock-entry :name "A Stack of Unread Memos (Hardbound)" :base-price 800
                                 :payload #'make-stack-of-unread-memos)
        (make-vendor-stock-entry :name "Red Swingline Stapler" :base-price 650
                                 :payload #'make-red-swingline-stapler)
        (make-vendor-stock-entry :name "3-Foot Ethernet Cable (Cat 6)" :base-price 700
                                 :payload #'make-three-foot-ethernet-cable)
        (make-vendor-stock-entry :name "Startup Green T-Shirt" :base-price 700
                                 :payload #'make-startup-green-t-shirt)
        (make-vendor-stock-entry :name "Blue-Light Blocking Glasses" :base-price 750
                                 :payload #'make-blue-light-blocking-glasses)
        (make-vendor-stock-entry :name "Patagonia Fleece Vest" :base-price 900
                                 :payload #'make-patagonia-fleece-vest)
        (make-vendor-stock-entry :name "AWS Certified Solutions Architect Plaque" :base-price 950
                                 :payload #'make-aws-certified-solutions-architect-plaque)
        (make-vendor-stock-entry :name "Headphones of Noise-Canceling" :base-price 1000
                                 :payload #'make-headphones-of-noise-canceling)
        (make-vendor-stock-entry :name "Telescoping Pointer (Laser Inactive)" :base-price 1100
                                 :payload #'make-telescoping-pointer)
        (make-vendor-stock-entry :name "Unwashed Hoodie" :base-price 1200
                                 :payload #'make-unwashed-hoodie)
        (make-vendor-stock-entry :name "Whiteboard Marker of Dominance" :base-price 1250
                                 :payload #'make-whiteboard-marker-of-dominance)
        (make-vendor-stock-entry :name "Branded Corporate Yeti Mug" :base-price 1300
                                 :payload #'make-branded-corporate-yeti-mug)
        (make-vendor-stock-entry :name "Razor-Sharp Aluminum Mousepad" :base-price 1400
                                 :payload #'make-razor-sharp-aluminum-mousepad)
        (make-vendor-stock-entry :name "Mechanical Keyboard (Cherry MX Blue)" :base-price 1500
                                 :payload #'make-mechanical-keyboard)
        (make-vendor-stock-entry :name "Rubber Band Gatling Gun" :base-price 1600
                                 :payload #'make-rubber-band-gatling-gun)
        (make-vendor-stock-entry :name "Laser Pointer of Redirection" :base-price 1600
                                 :payload #'make-laser-pointer-of-redirection)
        (make-vendor-stock-entry :name "Nerf Retaliator (Office Modded)" :base-price 1700
                                 :payload #'make-nerf-retaliator)
        (make-vendor-stock-entry :name "Ironed Button-Down" :base-price 1800
                                 :payload #'make-ironed-button-down)
        (make-vendor-stock-entry :name "YubiKey of Second Factors" :base-price 2000
                                 :payload #'make-yubikey-of-second-factors)
        (make-vendor-stock-entry :name "Agile Scrum Master Certificate" :base-price 2000
                                 :payload #'make-agile-scrum-master-certificate)
        (make-vendor-stock-entry :name "Stack Overflow Plagiarized Script" :base-price 2050
                                 :payload #'make-stack-overflow-plagiarized-script)
        (make-vendor-stock-entry :name "Megaphone of \"Let's Take This Offline\"" :base-price 2100
                                 :payload #'make-megaphone-of-lets-take-this-offline)
        (make-vendor-stock-entry :name "Keyboard of Kinesis (Epic)" :base-price 2200
                                 :payload #'make-keyboard-of-kinesis)
        (make-vendor-stock-entry :name "Can of Compressed Air" :base-price 2200
                                 :payload #'make-can-of-compressed-air)
        (make-vendor-stock-entry :name "USB Drive Shuriken" :base-price 2300
                                 :payload #'make-usb-drive-shuriken)
        (make-vendor-stock-entry :name "Lanyard of the VIP" :base-price 2400
                                 :payload #'make-lanyard-of-the-vip)
        (make-vendor-stock-entry :name "Severed Server Rack Rail" :base-price 2600
                                 :payload #'make-severed-server-rack-rail)
        (make-vendor-stock-entry :name "\"Reply-All\" Blunderbuss" :base-price 2800
                                 :payload #'make-reply-all-blunderbuss)
        (make-vendor-stock-entry :name "HR Whistleblower" :base-price 3200
                                 :payload #'make-hr-whistleblower))
  "The fixed catalog every VENDOR-FIXTURE sells from (FUTURE_PLANS.md
§10) -- unlike *RDESCENT-ITEM-SPAWN-TABLE*/*RDESCENT-MONSTER-SPAWN-
TABLE* (each a *weighted, depth-gated* table SPAWN-TABLE-CHOICE draws
one entry from at random), every entry here is *simultaneously* for
sale, indexed positionally (0-based) by PURCHASE-COMMAND's own
ITEM-INDEX -- a vending machine's whole point is that you can see and
choose exactly what you're buying, not that it's randomly dispensed.
BASE-PRICEs are set relative to a Stock Option's own 1-10000 RSU
windfall range (see MAKE-GROUND-STOCK-OPTION): a Kombucha (this game's
cheapest, most plentiful consumable) is 50 RSU, then the fixed
consumables (300-500), then the §13 armory stretches from everyday
office junk up through artifact-tier gear. Every entry here is a fresh
factory (or :KOMBUCHA keyword) so purchases never share instances
across players. See VENDOR-ITEM-PRICE for how a given player's own
SYNERGY further adjusts each of these before it's actually charged.
Defined here, after the MAKE-* factories it references (rather than
immediately alongside VENDOR-FIXTURE/VENDOR-STOCK-ENTRY earlier in this
file), because each entry's own #'-quoted PAYLOAD function must
already be FBOUNDP at LOAD-TIME -- this is a compiled FASL, not an
interpreted read-eval-print loop, so top-level forms run in file order
at load time.")


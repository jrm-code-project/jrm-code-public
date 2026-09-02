// Client engine for "Recursive Descent", a server-authoritative
// web-based roguelike. Vanilla ES6, no frameworks/build step -- loaded
// directly by /rdescent.html (see RENDER-RDESCENT-PAGE in views.lisp).
//
// All game state lives on the server; this script's only jobs are:
//   1. Open an authenticated WebSocket connection to /ws/rdescent.ws.
//   2. Apply the server's "update message-log" / "update playing-field"
//      packets to the DOM.
//   3. Forward arrow-key input to the server as move commands.
//   4. Drive a purely client-side UI state machine ('normal' /
//      'inventory' / 'equipment' / 'targeting') for the inventory
//      modal, the equip-menu/unequip-slot-picker modal, and item-
//      targeting cursor -- see UISTATE/HANDLEKEYDOWN.
(function () {
  'use strict';

  const RECONNECT_BASE_DELAY_MS = 1000;
  const RECONNECT_MAX_DELAY_MS = 30000;

  // Fallback grid dimensions used only until the first "playing-field"
  // packet arrives (see APPLYSERVERPACKET), which then overwrites
  // FIELDWIDTH/FIELDHEIGHT below with the server's own authoritative
  // *RDESCENT-FIELD-WIDTH*/*RDESCENT-FIELD-HEIGHT* values -- so the
  // client never needs its own independently-maintained copy of these
  // constants that could silently drift out of sync with the server.
  const DEFAULT_FIELD_WIDTH = 100;
  const DEFAULT_FIELD_HEIGHT = 33;

  const ARROW_KEY_DIRECTIONS = {
    ArrowUp: 'up',
    ArrowDown: 'down',
    ArrowLeft: 'left',
    ArrowRight: 'right',
  };

  const messageLogEl = document.getElementById('message-log');
  const playingFieldEl = document.getElementById('playing-field-grid');
  const statsDepthEl = document.getElementById('stats-depth');
  const statsRoomEl = document.getElementById('stats-room');
  const statsHpBarEl = document.getElementById('stats-hp-bar');
  const statsDmgBarEl = document.getElementById('stats-dmg-bar');
  const statsHpTextEl = document.getElementById('stats-hp-text');
  const statsXpEl = document.getElementById('stats-xp');
  const statsRsuEl = document.getElementById('stats-rsu');
  const statsKombuchaEl = document.getElementById('stats-kombucha');
  const statsEquipmentEl = document.getElementById('stats-equipment');
  // The seven "Corporate RPG Stats" (see ENTITY's docstring/ROLL-STAT,
  // rdescent/mechanics.lisp): each key here is both the suffix of its
  // #val-<key> DOM node id (see RENDER-RDESCENT-PAGE's
  // #player-corporate-stats markup, views.lisp) and of its
  // "val-<key>" field in the "player-stats" packet (see
  // RDESCENT-PLAYER-STATS-PACKET, rdescent/server.lisp) -- looped over
  // below in APPLYSERVERPACKET rather than spelled out seven times.
  const CORPORATE_STAT_KEYS = [
    'bandwidth', 'pivot', 'caffeine-tolerance', 'domain-knowledge',
    'seniority', 'synergy', 'hygiene',
  ];
  const corporateStatEls = {};
  CORPORATE_STAT_KEYS.forEach((key) => {
    corporateStatEls[key] = document.getElementById('val-' + key);
  });
  const gameEl = document.getElementById('rdescent-game');
  const messageLogModalEl = document.getElementById('message-log-modal');
  const messageLogModalBodyEl = document.getElementById('message-log-modal-body');
  const inventoryModalEl = document.getElementById('inventory-modal');
  const inventoryModalBodyEl = document.getElementById('inventory-modal-body');
  const equipmentModalEl = document.getElementById('equipment-modal');
  const equipmentModalBodyEl = document.getElementById('equipment-modal-body');
  const legendModalEl = document.getElementById('legend-modal');
  const legendModalBodyEl = document.getElementById('legend-modal-body');
  const plaqueModalEl = document.getElementById('plaque-modal');
  const plaqueModalBodyEl = document.getElementById('plaque-modal-body');
  const targetingCursorEl = document.getElementById('targeting-cursor');

  let socket = null;
  let reconnectAttempts = 0;
  let reconnectTimer = null;
  // Set just before the page navigates away/reloads (see the
  // 'pagehide' listener near the bottom of this file). Guards
  // SCHEDULERECONNECT so a reconnect timer never fires into a
  // document that's already being torn down, and lets us send a
  // clean WebSocket close handshake (code 1000) instead of letting
  // the browser sever the TCP connection abruptly -- an abrupt
  // severance racing against the *new* page's first CONNECT() call
  // on reload is what produces Chromium's "Close received after
  // close" error and a stuck reconnect-loop (see rdescent.js's own
  // 'pagehide' handler for the full explanation).
  let isUnloading = false;
  // Latest full (up to 50-entry) message-log HTML from the server,
  // refreshed on every message-log packet regardless of whether the
  // modal is currently open, so it's ready to show the instant 'v' is
  // pressed rather than needing a round-trip at toggle time.
  let latestHistoryHtml = '';

  // Client-side UI state machine layered on top of the server-
  // authoritative game state, driving the inventory modal, the
  // equipment modal, and the item-targeting cursor. 'normal' is
  // ordinary play (arrow keys move the player); 'inventory' is the
  // inventory modal open, arrow keys move INVENTORYSELECTEDINDEX ('e'
  // equips the selected item if it's EQUIPPABLE, 'd' drops it);
  // 'equipment' is the equipment modal open (opened by 'u'), arrow
  // keys move EQUIPMENTSELECTEDINDEX and Enter sends an "unequip"
  // command for the selected slot; 'targeting' is both modals closed
  // but an item pending, arrow keys move the reticle at (TARGETINGX,
  // TARGETINGY) and Enter fires PENDINGITEMINDEX at it. See
  // HANDLEKEYDOWN.
  let uiState = 'normal';
  // Latest inventory display entries from the server's "inventory"
  // packet (see APPLYSERVERPACKET), used to render #inventory-modal-body.
  // Each entry is { name, count, index }: NAME/COUNT are for display
  // (one entry per distinct item name, COUNT how many the player
  // carries -- see GROUP-INVENTORY-FOR-DISPLAY in rdescent/entities.lisp),
  // while INDEX is the raw position in the player's own INVENTORY list
  // that a "use-item"/"drop" command must reference to act on one
  // instance of that entry.
  let inventoryItems = [];
  let inventorySelectedIndex = 0;
  // Latest equipment-slot entries from the "player-stats" packet's
  // "equipment" field (see APPLYSERVERPACKET), used both to render
  // #stats-equipment in the side panel every tick and to populate
  // #equipment-modal-body when the 'u' key opens the unequip-slot
  // picker. Each entry is { slot, name }: SLOT is the fixed
  // uppercase slot keyword name (e.g. "WEAPON", "OFF-HAND") echoed
  // straight from RDESCENT-PLAYER-STATS-PACKET's own PRINC-TO-STRING,
  // and NAME is the occupying item's GET-ITEM-NAME or null if empty.
  let equipmentSlots = [];
  let equipmentSelectedIndex = 0;
  // The inventory index chosen (via Enter) before entering 'targeting'
  // mode; NIL-equivalent (null) whenever not actively targeting.
  let pendingItemIndex = null;
  let targetingX = 0;
  let targetingY = 0;
  // The playing field's own dimensions, refreshed every tick from the
  // "playing-field" packet's "width"/"height" fields (see
  // APPLYSERVERPACKET) -- kept in a variable rather than a constant so
  // they always match the server's own *RDESCENT-FIELD-WIDTH*/
  // *RDESCENT-FIELD-HEIGHT*, used below only to clamp the targeting
  // cursor's movement to the field's true bounds.
  let fieldWidth = DEFAULT_FIELD_WIDTH;
  let fieldHeight = DEFAULT_FIELD_HEIGHT;
  // Player's own current grid position, refreshed every tick from the
  // "player-stats" packet's "x"/"y" fields (see APPLYSERVERPACKET) --
  // the client has no independent notion of this otherwise, since all
  // game state is server-authoritative. Used to seed the targeting
  // cursor at the player's own position (see ENTERTARGETINGMODE)
  // rather than an arbitrary field-center guess.
  let playerX = Math.floor(DEFAULT_FIELD_WIDTH / 2);
  let playerY = Math.floor(DEFAULT_FIELD_HEIGHT / 2);

  /**
   * Show or hide the message-history modal (see #message-log-modal in
   * views.lisp's RENDER-RDESCENT-PAGE). Toggled by the 'v' key (see
   * handleKeydown): pressing 'v' while the modal is hidden opens it
   * (populating it with the latest LATEST-HISTORY-HTML and moving
   * focus into its scrollable body so arrow keys scroll history
   * instead of moving the player); pressing 'v' again, or Escape,
   * closes it and returns focus to the game so movement input resumes
   * immediately.
   */
  function toggleMessageLogModal() {
    if (!messageLogModalEl) return;
    const opening = messageLogModalEl.hasAttribute('hidden');
    if (opening) {
      if (messageLogModalBodyEl) messageLogModalBodyEl.innerHTML = latestHistoryHtml;
      messageLogModalEl.removeAttribute('hidden');
      if (messageLogModalBodyEl) messageLogModalBodyEl.focus();
    } else {
      messageLogModalEl.setAttribute('hidden', '');
      if (gameEl) gameEl.focus();
    }
  }

  /**
   * Show or hide the keybinding legend modal (see #legend-modal in
   * views.lisp's RENDER-RDESCENT-PAGE). Toggled by the '?' key (see
   * handleKeydown), dismissed either by pressing '?' again or by
   * Escape. Unlike TOGGLEMESSAGELOGMODAL/OPENINVENTORYMODAL, there is
   * no body content to populate -- #legend-modal-body is static
   * markup written directly into the page, since the set of
   * keybindings never changes at runtime.
   */
  function toggleLegendModal() {
    if (!legendModalEl) return;
    const opening = legendModalEl.hasAttribute('hidden');
    if (opening) {
      legendModalEl.removeAttribute('hidden');
      if (legendModalBodyEl) legendModalBodyEl.focus();
    } else {
      legendModalEl.setAttribute('hidden', '');
      if (gameEl) gameEl.focus();
    }
  }

  /**
   * Open the Commemorative Plaque modal (see #plaque-modal in
   * views.lisp's RENDER-RDESCENT-PAGE), populating its body with TEXT
   * and moving focus into it. Unlike TOGGLEMESSAGELOGMODAL/
   * TOGGLELEGENDMODAL, this is never toggled by a client keypress --
   * it is pushed open by the server itself, the instant a one-shot
   * "plaque" packet arrives (see APPLYSERVERPACKET), the moment a
   * player reads a final-level plaque via the 't' interact command
   * (see INTERACT-WITH-FIXTURE's own PLAQUE-FIXTURE method,
   * rdescent/actions.lisp). Dismissed only by Escape (see
   * HANDLEKEYDOWN/CLOSEPLAQUEMODAL) -- there is no toggling keypress
   * that reopens it, since the server only ever sends that packet
   * once per read.
   */
  function showPlaqueModal(text) {
    if (!plaqueModalEl) return;
    if (plaqueModalBodyEl) plaqueModalBodyEl.textContent = text;
    plaqueModalEl.removeAttribute('hidden');
    if (plaqueModalBodyEl) plaqueModalBodyEl.focus();
  }

  /** Close the Commemorative Plaque modal and return focus to the game. */
  function closePlaqueModal() {
    if (!plaqueModalEl) return;
    plaqueModalEl.setAttribute('hidden', '');
    if (gameEl) gameEl.focus();
  }

  /**
   * (Re)render #inventory-modal-body from INVENTORYITEMS, one
   * ".inventory-item" row per entry (NAME, with a \" (xN)\" suffix
   * when COUNT > 1), marking INVENTORYSELECTEDINDEX's row ".selected"
   * (see the matching CSS in style.css). Called both when the modal
   * is opened and whenever the selection or the underlying item list
   * changes.
   */
  function renderInventoryModal() {
    if (!inventoryModalBodyEl) return;
    if (inventoryItems.length === 0) {
      inventoryModalBodyEl.innerHTML = '<div class="inventory-item">(empty)</div>';
      return;
    }
    inventoryModalBodyEl.innerHTML = inventoryItems
      .map((item, index) => {
        const cls = index === inventorySelectedIndex ? 'inventory-item selected' : 'inventory-item';
        const label = item.count > 1 ? `${item.name} (x${item.count})` : item.name;
        return `<div class="${cls}">${label}</div>`;
      })
      .join('');
  }

  /** Open the inventory modal, entering 'inventory' UI state. */
  function openInventoryModal() {
    if (!inventoryModalEl) return;
    uiState = 'inventory';
    inventorySelectedIndex = 0;
    renderInventoryModal();
    inventoryModalEl.removeAttribute('hidden');
    if (inventoryModalBodyEl) inventoryModalBodyEl.focus();
  }

  /** Close the inventory modal, returning to 'normal' UI state and
   * game focus. */
  function closeInventoryModal() {
    if (inventoryModalEl) inventoryModalEl.setAttribute('hidden', '');
    uiState = 'normal';
    if (gameEl) gameEl.focus();
  }

  /**
   * (Re)render #stats-equipment from EQUIPMENTSLOTS, a plain list of
   * "<Slot>: <item name or (empty)> (<durability>/<max-durability>)"
   * rows -- mirrors the plain-text style of #stats-kombucha/#stats-xp
   * rather than the richer .inventory-item markup used by the modal
   * below, since the side panel has much less room. Called every
   * "player-stats" tick (see APPLYSERVERPACKET).
   */
  function renderStatsEquipment() {
    if (!statsEquipmentEl) return;
    statsEquipmentEl.innerHTML = equipmentSlots
      .map((entry) => `<div>${entry.slot}: ${formatEquipmentSlotLabel(entry)}</div>`)
      .join('');
  }

  /**
   * Return the display label for one EQUIPMENTSLOTS entry: just
   * "(empty)" for an empty slot, or "<item name> (<durability>/<max-
   * durability>)" for an occupied one -- shared by RENDERSTATSEQUIPMENT
   * and RENDEREQUIPMENTMODAL so both stay in sync.
   */
  function formatEquipmentSlotLabel(entry) {
    if (!entry.name) return '(empty)';
    const durability = entry.durability != null && entry['max-durability'] != null
      ? ` (${entry.durability}/${entry['max-durability']})`
      : '';
    return `${entry.name}${durability}`;
  }

  /**
   * (Re)render #equipment-modal-body from EQUIPMENTSLOTS, one
   * ".inventory-item" row per slot (reusing the inventory modal's own
   * CSS class so the two modals look consistent), marking
   * EQUIPMENTSELECTEDINDEX's row ".selected". Called both when the
   * modal is opened and whenever the selection or the underlying
   * equipment list changes.
   */
  function renderEquipmentModal() {
    if (!equipmentModalBodyEl) return;
    if (equipmentSlots.length === 0) {
      equipmentModalBodyEl.innerHTML = '<div class="inventory-item">(no equipment slots)</div>';
      return;
    }
    equipmentModalBodyEl.innerHTML = equipmentSlots
      .map((entry, index) => {
        const cls = index === equipmentSelectedIndex ? 'inventory-item selected' : 'inventory-item';
        const label = `${entry.slot}: ${formatEquipmentSlotLabel(entry)}`;
        return `<div class="${cls}">${label}</div>`;
      })
      .join('');
  }

  /** Open the equipment modal (the unequip-slot picker), entering
   * 'equipment' UI state. */
  function openEquipmentModal() {
    if (!equipmentModalEl) return;
    uiState = 'equipment';
    equipmentSelectedIndex = 0;
    renderEquipmentModal();
    equipmentModalEl.removeAttribute('hidden');
    if (equipmentModalBodyEl) equipmentModalBodyEl.focus();
  }

  /** Close the equipment modal, returning to 'normal' UI state and
   * game focus. */
  function closeEquipmentModal() {
    if (equipmentModalEl) equipmentModalEl.setAttribute('hidden', '');
    uiState = 'normal';
    if (gameEl) gameEl.focus();
  }

  /**
   * Transition from 'inventory' to 'targeting' mode for the item at
   * ITEMINDEX: hide the inventory modal (without returning to
   * 'normal'), remember ITEMINDEX as PENDINGITEMINDEX, and show the
   * targeting cursor starting at the player's own current position
   * (PLAYERX/PLAYERY, kept fresh every tick from the server's
   * "player-stats" packet -- see APPLYSERVERPACKET) as a sensible
   * initial aim point the player can then move with arrow keys.
   */
  function enterTargetingMode(itemIndex) {
    if (inventoryModalEl) inventoryModalEl.setAttribute('hidden', '');
    uiState = 'targeting';
    pendingItemIndex = itemIndex;
    targetingX = playerX;
    targetingY = playerY;
    if (targetingCursorEl) targetingCursorEl.removeAttribute('hidden');
    updateTargetingCursor();
  }

  /** Cancel out of 'targeting' mode back to 'normal' play, hiding the
   * cursor and forgetting PENDINGITEMINDEX. */
  function exitTargetingMode() {
    uiState = 'normal';
    pendingItemIndex = null;
    if (targetingCursorEl) targetingCursorEl.setAttribute('hidden', '');
    if (gameEl) gameEl.focus();
  }

  /**
   * Reposition #targeting-cursor over (TARGETINGX, TARGETINGY) using
   * ch/em offsets matching #playing-field-inner's own 10px padding
   * and monospace grid-cell size (see style.css's #targeting-cursor
   * rule). This stays accurate regardless of viewport width because
   * #targeting-cursor and #playing-field-grid are both direct
   * children of #playing-field-inner, which is sized to the grid's
   * own content width and centered via margin:auto -- see that CSS
   * rule's comment.
   */
  function updateTargetingCursor() {
    if (!targetingCursorEl) return;
    targetingCursorEl.style.left = `calc(10px + ${targetingX}ch)`;
    targetingCursorEl.style.top = `calc(10px + ${targetingY * 1.15}em)`;
  }

  /**
   * Read the JWT the server embedded for us in the hidden data div
   * (see /rdescent.html's #rdescent-data[data-jwt]). Returns "" if the
   * div is missing or carries no token (e.g. the visitor loaded the
   * page without presenting a JWT).
   */
  function getJwtToken() {
    const dataEl = document.getElementById('rdescent-data');
    return (dataEl && dataEl.dataset.jwt) || '';
  }

  /** Build the ws:// or wss:// URL for /ws/rdescent.ws, matching the page's own
   * protocol/host/port, with the JWT attached as a query parameter. */
  function buildSocketUrl() {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const token = encodeURIComponent(getJwtToken());
    return `${protocol}//${window.location.host}/ws/rdescent.ws?token=${token}`;
  }

  /**
   * Apply one server packet to the DOM. Expected shapes:
   *   { "target": "message-log" | "playing-field", "html": "<...>" }
   *     ("message-log" packets also carry "history-html", the last 50
   *     entries, used to populate/refresh the #message-log-modal
   *     toggled by the 'v' key -- see TOGGLEMESSAGELOGMODAL. "playing-
   *     field" packets also carry "width"/"height", the server's own
   *     authoritative grid dimensions -- refreshed into
   *     FIELDWIDTH/FIELDHEIGHT every tick, used only to clamp the
   *     targeting cursor's movement to the field's true bounds, so
   *     the client never needs a second, independently-maintained
   *     copy of these constants that could silently drift out of sync
   *     with the server's own values.)
   * { "target": "player-stats", "depth-html": "<...>", "room-html": "<...>",
   *     "hp-pct": N,
   *     "dmg-pct": N, "hp-text": "<...>", "xp-html": "<...>",
   *     "rsu-html": "<...>", "kombucha-html": "<...>",
   *     "equipment": [{"slot": "WEAPON", "name": "..." | null,
   *     "durability": N | null, "max-durability": N | null}, ...],
   *     "val-bandwidth": "<N>", "val-pivot": "<N>",
   *     "val-caffeine-tolerance": "<N>", "val-domain-knowledge": "<N>",
   *     "val-seniority": "<N>", "val-synergy": "<N>",
   *     "val-hygiene": "<N>", "x": N, "y": N }
   *   { "target": "inventory", "items": [{"name": "Scroll of PIP",
   *     "count": 2, "index": 0, "equippable": false}, ...] }
   *     refreshes INVENTORYITEMS and, if the inventory modal is
   *     currently open, re-renders it in place (see
   *     RENDERINVENTORYMODAL).
   * player-stats packets update the side panel's *persistent* DOM
   * nodes (#stats-depth/#stats-room/#stats-hp-bar/#stats-dmg-bar/#stats-hp-text/
   * #stats-xp/#stats-rsu/#stats-kombucha/#stats-equipment/the seven
   * #val-... Corporate RPG Stat spans, see CORPORATESTATELS) in place -- via
   * style.width/textContent/innerHTML on
   * the existing elements -- rather than replacing them wholesale, so
   * the HP bar's CSS width transition has a previous value to animate
   * from (an innerHTML replacement would recreate the spans from
   * scratch every tick, giving CSS nothing to transition between).
   * They also refresh PLAYERX/PLAYERY from "x"/"y" every tick, which
   * ENTERTARGETINGMODE uses to seed the targeting cursor at the
   * player's own current position.
   * Unrecognized packets are logged and otherwise ignored, so a future
   * server-side protocol addition doesn't crash the client.
   */
  function applyServerPacket(packet) {
    if (!packet || typeof packet.target !== 'string') {
      console.warn('rdescent: ignoring malformed packet', packet);
      return;
    }
    switch (packet.target) {
      case 'message-log':
        if (messageLogEl && typeof packet.html === 'string') messageLogEl.innerHTML = packet.html;
        if (typeof packet['history-html'] === 'string') {
          latestHistoryHtml = packet['history-html'];
          // If the modal is currently open, keep it live-updated too.
          if (messageLogModalBodyEl && messageLogModalEl && !messageLogModalEl.hasAttribute('hidden')) {
            messageLogModalBodyEl.innerHTML = latestHistoryHtml;
          }
        }
        break;
      case 'playing-field':
        if (playingFieldEl && typeof packet.html === 'string') playingFieldEl.innerHTML = packet.html;
        if (typeof packet.width === 'number') fieldWidth = packet.width;
        if (typeof packet.height === 'number') fieldHeight = packet.height;
        break;
      case 'player-stats':
        if (statsDepthEl && typeof packet['depth-html'] === 'string') statsDepthEl.innerHTML = packet['depth-html'];
        if (statsRoomEl && typeof packet['room-html'] === 'string') statsRoomEl.innerHTML = packet['room-html'];
        if (statsHpBarEl && typeof packet['hp-pct'] === 'number') statsHpBarEl.style.width = packet['hp-pct'] + '%';
        if (statsDmgBarEl && typeof packet['dmg-pct'] === 'number') statsDmgBarEl.style.width = packet['dmg-pct'] + '%';
        if (statsHpTextEl && typeof packet['hp-text'] === 'string') statsHpTextEl.textContent = packet['hp-text'];
        if (statsXpEl && typeof packet['xp-html'] === 'string') statsXpEl.innerHTML = packet['xp-html'];
        if (statsRsuEl && typeof packet['rsu-html'] === 'string') statsRsuEl.innerHTML = packet['rsu-html'];
        if (statsKombuchaEl && typeof packet['kombucha-html'] === 'string') statsKombuchaEl.innerHTML = packet['kombucha-html'];
        if (Array.isArray(packet.equipment)) {
          equipmentSlots = packet.equipment;
          if (equipmentSelectedIndex >= equipmentSlots.length) {
            equipmentSelectedIndex = Math.max(0, equipmentSlots.length - 1);
          }
          renderStatsEquipment();
          if (uiState === 'equipment') renderEquipmentModal();
        }
        CORPORATE_STAT_KEYS.forEach((key) => {
          const el = corporateStatEls[key];
          const value = packet['val-' + key];
          if (el && typeof value === 'string') el.textContent = value;
        });
        if (typeof packet.x === 'number') playerX = packet.x;
        if (typeof packet.y === 'number') playerY = packet.y;
        break;
      case 'inventory':
        if (Array.isArray(packet.items)) {
          inventoryItems = packet.items;
          if (inventorySelectedIndex >= inventoryItems.length) {
            inventorySelectedIndex = Math.max(0, inventoryItems.length - 1);
          }
          if (uiState === 'inventory') renderInventoryModal();
        }
        break;
      case 'plaque':
        if (typeof packet.text === 'string') showPlaqueModal(packet.text);
        break;
      case 'save-payload':
        if (typeof packet.payload === 'string') {
          window.localStorage.setItem('rdescent_save', packet.payload);
          console.info('rdescent: Game state saved to localStorage.');
          if (messageLogEl) {
             // We drop it into the DOM immediately so the player sees it instantly...
             messageLogEl.innerHTML = '<span style="color:#a3be8c">*** Game saved to browser ***</span><br>' + messageLogEl.innerHTML;
             // ...but because the server's tick loop pushes a brand new message-log HTML 
             // payload every second or two, it blindly overwrites our client-side injection.
             // We need to inject this into the *server's* message log so it sticks.
             // The easiest way is to just send a message-log command back to the server.
             sendMessageLogCommand('*** Game saved to browser ***');
          }
        }
        break;
    }
  }

  function sendMoveCommand(direction) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'move', direction: direction }));
    }
  }

  function sendUseStairsCommand() {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'use-stairs' }));
    }
  }

  function sendDrinkCommand() {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'drink' }));
    }
  }

  function sendUseItemCommand(itemIndex, targetX, targetY) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({
        action: 'use-item',
        'item-index': itemIndex,
        'target-x': targetX,
        'target-y': targetY,
      }));
    }
  }

  function sendGrabCommand() {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'grab' }));
    }
  }

  function sendInteractCommand() {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'interact' }));
    }
  }

  function sendDropCommand(itemIndex) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'drop', 'item-index': itemIndex }));
    }
  }

  function sendEquipCommand(itemIndex) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'equip', 'item-index': itemIndex }));
    }
  }

  // SLOT must be one of the lowercase strings PARSE-RDESCENT-COMMAND
  // recognizes (rdescent/commands.lisp): "weapon"/"body"/"head"/
  // "off-hand" -- callers here always derive it by lower-casing an
  // EQUIPMENTSLOTS entry's own uppercase "slot" field (echoed straight
  // from RDESCENT-PLAYER-STATS-PACKET's PRINC-TO-STRING), so it's
  // never hand-typed. UNEQUIP-ITEM (rdescent/actions.lisp) itself
  // handles both an empty slot ("nothing equipped there") and a
  // cursed occupant ("won't come off!") gracefully with a message-log
  // line and no turn spent, so this is a safe no-op to send even if
  // the selected slot turns out to be empty or cursed.
  function sendUnequipCommand(slot) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'unequip', slot: slot }));
    }
  }

  function sendSaveCommand() {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'save' }));
    }
  }

  // Lets other front-end features (not just the SAVE flow above) ask the
  // server to append an arbitrary line to the message log -- the server
  // (MESSAGE-LOG-COMMAND, rdescent/commands.lisp) truncates and sanitizes
  // TEXT before logging it, so callers here don't need to pre-validate it.
  function sendMessageLogCommand(text) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'message-log', text: text }));
    }
  }

  function sendRestoreCommand(payload) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify({ action: 'restore', payload: payload }));
    }
  }

  // Triggered only by an explicit 'r'/'R' keypress (see handleKeydown) --
  // this game never restores automatically. Reads the base64 save blob
  // PACK-SAVE-STATE previously wrote to the 'rdescent_save' localStorage
  // key (see the 'save-payload' case above), and as long as it's under
  // 64KB, sends it to the server as a RESTORE-COMMAND followed by a
  // "*** Game Restored ***" message-log line (queued in that order, so
  // the confirmation lands in the freshly-restored game's own log, not
  // the old one it's about to replace). An oversized blob is rejected
  // client-side instead -- never sent -- with a "*** Error: Saved Game
  // too large ***" message-log line. If there's no saved game at all,
  // this is a silent no-op.
  function restoreGame() {
    const payload = window.localStorage.getItem('rdescent_save');
    if (!payload) return;
    if (payload.length >= 64 * 1024) {
      sendMessageLogCommand('*** Error: Saved Game too large ***');
      return;
    }
    sendRestoreCommand(payload);
    sendMessageLogCommand('*** Game Restored ***');
  }

  function handleKeydown(event) {
    if (event.key === 'v' || event.key === 'V') {
      event.preventDefault();
      toggleMessageLogModal();
      return;
    }
    const modalOpen = messageLogModalEl && !messageLogModalEl.hasAttribute('hidden');
    if (modalOpen) {
      if (event.key === 'Escape') {
        event.preventDefault();
        toggleMessageLogModal();
        return;
      }
      // While the modal is open, arrow keys scroll its body instead of
      // moving the player -- the browser's own scrolling of a
      // focused, overflow:auto element already does this for free as
      // long as we don't preventDefault/forward the keystroke as a
      // move command.
      return;
    }

    const plaqueOpen = plaqueModalEl && !plaqueModalEl.hasAttribute('hidden');
    if (plaqueOpen) {
      if (event.key === 'Escape') {
        event.preventDefault();
        closePlaqueModal();
      }
      // Swallow all other keys while the plaque modal is open -- like
      // the legend modal, it has no toggling keypress of its own (see
      // SHOWPLAQUEMODAL's own docstring), only Escape closes it.
      return;
    }

    if (event.key === '?') {
      event.preventDefault();
      toggleLegendModal();
      return;
    }
    const legendOpen = legendModalEl && !legendModalEl.hasAttribute('hidden');
    if (legendOpen) {
      if (event.key === 'Escape') {
        event.preventDefault();
        toggleLegendModal();
      }
      // Swallow all other keys while the legend modal is open.
      return;
    }

    if (uiState === 'inventory') {
      if (event.key === 'Escape') {
        event.preventDefault();
        closeInventoryModal();
        return;
      }
      if (event.key === 'ArrowUp' || event.key === 'ArrowDown') {
        event.preventDefault();
        if (inventoryItems.length > 0) {
          const delta = event.key === 'ArrowUp' ? -1 : 1;
          inventorySelectedIndex = (inventorySelectedIndex + delta + inventoryItems.length) % inventoryItems.length;
          renderInventoryModal();
        }
        return;
      }
      if (event.key === 'Enter') {
        event.preventDefault();
        if (inventoryItems.length > 0) {
          const selected = inventoryItems[inventorySelectedIndex];
          if (selected.equippable) {
            // Every EQUIPPABLE-ITEM (RDESCENT/ENTITIES.LISP) is a
            // melee weapon or piece of armor -- there is no ranged
            // gear needing an aimed target, so Enter equips it
            // directly (mirroring the 'e' key) instead of entering
            // targeting mode, which only makes sense for a
            // TARGETED-ITEM/AREA-EFFECT-ITEM consumable.
            sendEquipCommand(selected.index);
            closeInventoryModal();
          } else {
            enterTargetingMode(selected.index);
          }
        }
        return;
      }
      if (event.key === 'd' || event.key === 'D') {
        event.preventDefault();
        if (inventoryItems.length > 0) {
          sendDropCommand(inventoryItems[inventorySelectedIndex].index);
          closeInventoryModal();
        }
        return;
      }
      if (event.key === 'e' || event.key === 'E') {
        event.preventDefault();
        if (inventoryItems.length > 0 && inventoryItems[inventorySelectedIndex].equippable) {
          sendEquipCommand(inventoryItems[inventorySelectedIndex].index);
          closeInventoryModal();
        }
        return;
      }
      // Swallow all other keys while the inventory modal is open.
      return;
    }

    if (uiState === 'equipment') {
      if (event.key === 'Escape') {
        event.preventDefault();
        closeEquipmentModal();
        return;
      }
      if (event.key === 'ArrowUp' || event.key === 'ArrowDown') {
        event.preventDefault();
        if (equipmentSlots.length > 0) {
          const delta = event.key === 'ArrowUp' ? -1 : 1;
          equipmentSelectedIndex = (equipmentSelectedIndex + delta + equipmentSlots.length) % equipmentSlots.length;
          renderEquipmentModal();
        }
        return;
      }
      if (event.key === 'Enter') {
        event.preventDefault();
        if (equipmentSlots.length > 0) {
          sendUnequipCommand(equipmentSlots[equipmentSelectedIndex].slot.toLowerCase());
          closeEquipmentModal();
        }
        return;
      }
      // Swallow all other keys while the equipment modal is open.
      return;
    }

    if (uiState === 'targeting') {
      if (event.key === 'Escape') {
        event.preventDefault();
        exitTargetingMode();
        return;
      }
      const direction = ARROW_KEY_DIRECTIONS[event.key];
      if (direction) {
        event.preventDefault();
        if (direction === 'up') targetingY = Math.max(0, targetingY - 1);
        if (direction === 'down') targetingY = Math.min(fieldHeight - 1, targetingY + 1);
        if (direction === 'left') targetingX = Math.max(0, targetingX - 1);
        if (direction === 'right') targetingX = Math.min(fieldWidth - 1, targetingX + 1);
        updateTargetingCursor();
        return;
      }
      if (event.key === 'Enter') {
        event.preventDefault();
        sendUseItemCommand(pendingItemIndex, targetingX, targetingY);
        exitTargetingMode();
        return;
      }
      // Swallow all other keys while targeting.
      return;
    }

    if (event.key === 'i' || event.key === 'I') {
      event.preventDefault();
      openInventoryModal();
      return;
    }
    if (event.key === 'u' || event.key === 'U') {
      event.preventDefault();
      openEquipmentModal();
      return;
    }
    if (event.key === '<' || event.key === '>') {
      event.preventDefault();
      sendUseStairsCommand();
      return;
    }
    if (event.key === 'k' || event.key === 'K') {
      event.preventDefault();
      sendDrinkCommand();
      return;
    }
    if (event.key === 'g' || event.key === 'G') {
      event.preventDefault();
      sendGrabCommand();
      return;
    }
    if (event.key === 't' || event.key === 'T') {
      event.preventDefault();
      sendInteractCommand();
      return;
    }
    if (event.key === 's' || event.key === 'S') {
      event.preventDefault();
      sendSaveCommand();
      return;
    }
    if (event.key === 'r' || event.key === 'R') {
      event.preventDefault();
      restoreGame();
      return;
    }
    const direction = ARROW_KEY_DIRECTIONS[event.key];
    if (!direction) return;
    // Prevent the browser window from scrolling on arrow-key presses.
    event.preventDefault();
    sendMoveCommand(direction);
  }


  function scheduleReconnect() {
    if (isUnloading || reconnectTimer) return;
    const delay = Math.min(
      RECONNECT_BASE_DELAY_MS * Math.pow(2, reconnectAttempts),
      RECONNECT_MAX_DELAY_MS
    );
    reconnectAttempts += 1;
    console.warn(`rdescent: connection lost, reconnecting in ${delay}ms`);
    reconnectTimer = window.setTimeout(() => {
      reconnectTimer = null;
      connect();
    }, delay);
  }

  function connect() {
    socket = new WebSocket(buildSocketUrl());

    socket.addEventListener('open', () => {
      reconnectAttempts = 0;
      console.info('rdescent: connected to /ws/rdescent.ws');
    });

    socket.addEventListener('message', (event) => {
      let packet;
      try {
        packet = JSON.parse(event.data);
      } catch (err) {
        console.error('rdescent: failed to parse server message', err, event.data);
        return;
      }
      applyServerPacket(packet);
    });

    socket.addEventListener('error', (event) => {
      console.error('rdescent: WebSocket error', event);
    });

    socket.addEventListener('close', () => {
      socket = null;
      scheduleReconnect();
    });
  }

  document.addEventListener('keydown', handleKeydown);
  connect();

  // On reload/navigate-away/tab-close, close our own socket cleanly (a
  // real WebSocket close handshake, code 1000) rather than letting the
  // browser sever the underlying TCP connection out from under it.
  // Also cancel any already-scheduled reconnect and set ISUNLOADING so
  // a reconnect timer that's mid-flight doesn't fire into a document
  // that's already gone. Without this, an abrupt severance here can
  // race against the *next* page load's own fresh CONNECT() call (to
  // the same wss:// URL) closely enough that Chromium logs "Close
  // received after close" and gets stuck endlessly reconnecting --
  // reported as "arrow keys don't work" after a page reload, since
  // every reconnect attempt keeps failing the same way.
  window.addEventListener('pagehide', () => {
    isUnloading = true;
    if (reconnectTimer) {
      window.clearTimeout(reconnectTimer);
      reconnectTimer = null;
    }
    if (socket) socket.close(1000, 'page unloading');
  });

  // Robustness: arrow-key input only reaches us while the game
  // container has focus. If the browser's focus drifts elsewhere (a
  // dev-tools panel, the address bar, an iframe, etc.) arrow keys stop
  // being delivered here at all -- from the player's perspective the
  // game "freezes" with no error, since there's nothing to catch. Grab
  // focus on load and reclaim it on any click inside the page, so a
  // stray focus loss self-heals on the player's very next interaction
  // rather than requiring them to notice and manually click the field.
  if (gameEl) {
    gameEl.focus();
    document.addEventListener('mousedown', () => gameEl.focus());
    window.addEventListener('focus', () => gameEl.focus());
  }
})();

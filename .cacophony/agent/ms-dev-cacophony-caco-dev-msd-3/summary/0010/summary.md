# Session summary — bd-e49551 slice 1: keyboard parity globals

## Goal

Workspace-view keyboard parity with the TUI: ship the global-scope
slice (Ctrl+Shift+P palette trigger, `?` help overlay,
discoverable binding registry) without blocking on pane-infra
contracts. Per-pane focus navigation (Ctrl-W h/j/k/l, Tab focus
cycle) needs the bd-232e03 pane tree consumable, so it lands in a
later slice.

## Bead(s)

- `bd-e49551` — [workspace-view] Keyboard parity with TUI

## Before state

- caco-web only bound Ctrl/Cmd+K for the command palette; the
  TUI uses Ctrl+Shift+P as its canonical trigger so muscle memory
  bounced.
- No keyboard help overlay; new operators had to read source to
  discover chords.
- Keybindings were scattered across the global keydown handler
  with no central registry.

## After state

- Ctrl/Cmd+Shift+P opens the command palette (Ctrl/Cmd+K
  retained for backwards compat).
- `?` opens a keyboard help overlay (`#keyboard-help-overlay`)
  that lists every registered chord grouped by scope. Esc closes
  it; backdrop click dismisses; pressing `?` again toggles.
- `window.KEYBOARD_BINDINGS` is the single-source-of-truth
  registry — downstream pane-infra beads append their own scoped
  chords without editing the global handler.
- New CSS section in style.css for `.keyboard-help-*`: `kbd`
  styling, scope headings, two-column table layout.

## Diff summary

- `crates/caco-web/static/app.js` (+96): keydown handler
  additions for the two new chords, plus the
  `toggleKeyboardHelp()` function and the `KEYBOARD_BINDINGS`
  registry.
- `crates/caco-web/static/style.css` (+47): help-overlay styling.
- `crates/caco-web/src/tests.rs` (+38): new test
  `app_js_keyboard_parity_contract_exposed` locking the contract
  (registry name, function name, two new chords, stable overlay
  id).
- 62/62 caco-web tests pass.

## Embedded artefacts

(none)

## Operator-takeaway

Slice 1 ships the discoverable parts of TUI parity. Slice 2
(per-pane focus nav, Ctrl-W chord prefix, terminal pane key
swallowing) needs bd-232e03's pane tree contracts to be
consumable from JS, so it can land as a separate small bead once
that ships. The KEYBOARD_BINDINGS registry was designed
explicitly so downstream panes can append their own bindings
without touching the global handler — a clean extension point
for the chunky parallel beads.

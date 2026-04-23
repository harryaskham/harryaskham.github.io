# Session summary — bd-f303f4 caco-web STT visual indicators in workspace chat pane

## Goal

Operator wants to direct the fleet by voice. The flaky existing STT
(scribble/whisper) needs **visible** mic-state indicators across both
TUI and web surfaces so the operator knows when speech is being heard
vs ignored. This bead is the web-side slice; bd-c16753 is the TUI
sibling.

Per Harry's Apr-23 note: "needs clear visual indicators for when
speech is correctly detected and our UX polish passes should test it
out with synthetic speech and ensure the live-voice-call flow works
for operator to guide fleet by talking with controllers."

## Bead(s)

- `bd-f303f4` — Visual STT indicators in caco-web workspace chat pane (P1)
- (parent epic: `bd-9496d1` xplat STT + voice-call-with-controller)
- (sibling: `bd-c16753` same indicators in TUI)
- (consumer of the wire format from the just-landed `bd-a17114`
  caco-stt-protocol crate — wire vocabulary `partial`/`final`/
  `silence`/`error`/`started`/`stopped` directly informed the
  WorkspaceSttMic event names)

## Before state

- caco-web workspace chat pane (bd-eaae6a) had no STT entry-point
- No mic widget, no visual indicators, no way for operator to dictate
  into the compose box
- Existing `crates/caco-daemon/src/scribble_stt.rs` covers the
  daemon-side engine but no browser-facing UX consumed it

## After state

- New `crates/caco-web/static/workspace-stt-mic.js` (~340 lines):
  idempotent `window.WorkspaceSttMic` namespace exposing
  `mount(hostEl, opts)` factory, `STATES` enum, `STATE_LABELS`,
  `browserSupportsSTT()` capability probe.
- Per-instance API: `start()`, `stop()`, `toggle()`, `destroy()`,
  `onPartial`, `onFinal`, `onState`, `onError`. Also dispatches a
  `stt-final` CustomEvent for non-callback consumers.
- Two modes: `push-to-talk` (hold Space, Esc to abort) and
  `always-on` (VAD-driven, auto-restart on engine end).
- Browser engine: webkit-prefixed SpeechRecognition with the standard
  unprefixed fallback. Firefox surfaces a clear 'browser STT
  unavailable' state instead of failing silently.
- New `crates/caco-web/static/workspace-stt-mic.css` (~140 lines):
  4-state coloured dot (grey/green/amber-pulsing/blue-slow-pulse/red),
  inline partial-transcript ghost text, brief final-commit flash for
  visible 'heard you' confirmation. `prefers-reduced-motion`
  respected throughout.
- `crates/caco-web/static/workspace-chat-pane.js` modified: auto-
  mounts mic widget if `WorkspaceSttMic` is loaded; on final-commit
  appends to compose textarea with smart spacing, focuses, moves
  caret to end. Wrapped in try/catch so a mic mount failure can
  NEVER break the chat pane. Per acceptance criterion 3, NEVER
  auto-sends — operator confirms with Enter.
- `crates/caco-web/static/workspace-chat-pane.css` modified: styles
  the new `.wcp-mic-row` container, with `:empty` collapse so layout
  doesn't shift when STT is unavailable.
- 4 new Rust embed-contract tests in `crates/caco-web/src/tests.rs`,
  all pure-Rust per bd-d5b850 perf concern (no node spawns):
  - `workspace_stt_mic_js_is_embedded` — pins the public surface,
    states, modes, engine probe, final-flash class, CustomEvent
  - `workspace_stt_mic_css_is_embedded` — pins all 4-state classes
    + reduced-motion + flash
  - `workspace_chat_pane_mounts_stt_mic_widget_bd_f303f4` — pins
    chat pane wiring + the no-auto-send invariant
  - `workspace_chat_pane_css_styles_mic_row` — pins mic-row CSS

## Diff summary

- Files: 6 touched (2 created, 4 modified)
  - `crates/caco-web/static/workspace-stt-mic.js` (new, ~340 lines)
  - `crates/caco-web/static/workspace-stt-mic.css` (new, ~140 lines)
  - `crates/caco-web/static/workspace-chat-pane.js` (+~38 lines)
  - `crates/caco-web/static/workspace-chat-pane.css` (+12 lines)
  - `crates/caco-web/src/tests.rs` (+4 tests, ~110 lines)
- Tests: +4 / -0
- Behavioural delta: the chat pane gains a mic widget when the new
  `workspace-stt-mic.js` is loaded. Existing chat pane behaviour is
  unchanged when the mic module is absent (the `.wcp-mic-row` div
  collapses via `:empty`).

## Operator-takeaway

This is the **visible operator confirmation** that voice direction is
working. Before this bead, scribble/whisper transcripts arrived in
silence — the operator had to guess whether the mic was even open.
Now: dot colour at-a-glance state, pulsing animation when speech is
detected, ghost-text partial that materialises as the operator
speaks, and a brief flash on the line when a final-commit happens.

Push-to-talk (Space) is the default because that's the safer mode for
voice direction: operator deliberately presses to speak, releases to
commit. Always-on mode is one button-press away for hands-free
sessions.

The mic widget is fully decoupled from the daemon-side engine — it
uses the browser SpeechRecognition API right now, but the same
WorkspaceSttMic surface can be re-pointed at the daemon's scribble
stream over WebSocket once bd-128ea6 stabilizes that path. The
`onFinal` / `onPartial` callback shape and the `stt-final` CustomEvent
are deliberately wire-shape-compatible with the bd-a17114
caco-stt-protocol crate's `StreamEvent::{Partial, Final}` variants.

Next sibling beads in this lane:
- bd-c16753 (TUI) — same 4-state indicators in speech_popup.rs
- bd-abd7ba — 'Test STT' button in speech-popup
- bd-07d590 — voice-call orchestration end-to-end

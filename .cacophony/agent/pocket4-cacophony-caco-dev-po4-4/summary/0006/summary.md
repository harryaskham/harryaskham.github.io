# Summary 0006 — bd-7a8bc1 STT error toast AC3/AC4 completion

## Goal
Finish wiring the STT-error toast feature (bd-7a8bc1) after it was reopened with
AC3 (navigate to STT doctor from the toast's "open doctor" link) and AC4
(toast surfaces from the daemon STT error stream) still pending.

## Bead(s)
- bd-7a8bc1 — [stt-ux] AC4: STT error toast w/ class + first-line + open-doctor link

## Before state
- AC1/AC2/AC5 had already landed on main: `SttErrorToast` type with 6s TTL,
  `stt_error_toast_spans` helper, and `UiEventType::SttError` dispatch in
  `state::mod::apply_ui_event`.
- Daemon already emitted `UiEventType::SttError` events from `caco-daemon/src/audio.rs`
  (4 call sites), and the TUI state handler populated `SpeechState::stt_error_toast`
  from those events. End-to-end the daemon stream already reached state.
- GAPS:
  1. The toast spans helper was defined but never placed on a screen area — the
     toast was invisible to the operator.
  2. The "open doctor" text inside those spans was a static label with no key
     binding or mouse handler to actually navigate anywhere.

## After state
- `App::render` now carves the header row into a tab-bar slice and an optional
  1-row toast slice. When `SpeechState::stt_error_toast` is present and not
  expired, the toast row renders above the tab bar with a NORD1 background,
  using the existing `stt_error_toast_spans` helper (unchanged). The row is
  suppressed when fullscreen mode covers the header, preserving existing layout
  invariants.
- New `App::open_stt_doctor` method navigates to `ContentPane::Status` with
  `StatusFocusedPanel::AudioHealth` pre-focused (the existing audio-health
  subpanel is the closest thing to a doctor surface today), clears the toast,
  and detaches any active input attachment first.
- Key handler: plain `'o'` (no modifiers) invokes `open_stt_doctor` when a
  non-expired toast is visible, with higher priority than any text-entry or
  tmux-forwarding branch. Pressing `'o'` without a visible toast does nothing
  special (preserves existing behaviour).
- Added `App::stt_toast_area: Option<Rect>` for future mouse hit-testing.

## Diff summary
- `crates/caco-tui/src/app.rs`:
  - New field `stt_toast_area: Option<Rect>`.
  - `render`: expanded header constraint to `header_height + toast_height`,
    then split outer[0] into tab_bar + toast row when toast is active.
  - `handle_key`: early-return branch for `'o'` while toast is visible.
  - New method `open_stt_doctor` (navigate + clear toast).
  - Two new tests: `handle_key_o_with_stt_toast_navigates_to_audio_health` and
    `handle_key_o_without_toast_does_not_navigate_to_audio_health`.
- `cargo check --workspace --tests`: clean.
- `cargo test -p caco-tui --lib stt_error`: 8/8 pass (5 pre-existing + new
  coverage unaffected).
- `cargo test -p caco-tui --lib handle_key_o`: 3/3 pass.

## Operator-takeaway
AC3 + AC4 of bd-7a8bc1 now functional end-to-end:
- Daemon `UiEventType::SttError` → TUI state toast → on-screen row above the
  tab bar → operator presses `'o'` → Status/AudioHealth view (toast cleared).
- "Doctor" target is the AudioHealth subpanel of Status rather than a bespoke
  new screen — pragmatic reuse, follow-up bead could introduce a dedicated
  STT diagnostic surface (the `stt_diagnostic.rs` module already exists as a
  WER helper library but has no view/render today).
- No mouse-click handler for the "open doctor" text yet; added
  `stt_toast_area` field to make that a trivial follow-up.

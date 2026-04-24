# Session summary — bd-7a8bc1 STT error toast (AC4 wiring)

## Goal

Wire the missing piece (AC4: daemon-side error stream) for the
STT-error-toast feature so the toast surface that AC1/2/3/5 already
shipped actually has a source.

## Bead(s)

- `bd-7a8bc1` — [stt-ux] AC4: STT error toast w/ class + first-line
  + open-doctor link

## Before state

- AC1 (`SpeechState::stt_error_toast` + `SttErrorToast` type with
  `new`, `is_expired`, `label`), AC2 (`stt_error_toast_spans` render
  with auto-dismiss), AC3 (`views/stt_diagnostic.rs` link target),
  and AC5 (render-spans test) had landed under a sibling agent's
  work on the parent bead bd-c16753.
- AC4 (the actual surface from daemon STT failures) was unimplemented:
  the toast had no source. The daemon's
  `handle_scribble_transcription` returned 4xx/5xx ErrorEnvelopes
  but never published anything to `state.ui_broadcast`.

## After state

- New `UiEventType::SttError` variant with `{ class, message }`
  payload. Class mirrors canonical `stt_event_class=` keys from the
  bd-128ea6 stabilization work (transcribe / decode / model_load /
  mic_perm / http_5xx).
- All 4 daemon error branches in `handle_scribble_transcription` now
  publish `SttError` before returning the HTTP error:
  scribble_not_configured (class=model_load), invalid base64
  (class=decode), spawn_blocking failure (class=transcribe), task
  panic (class=transcribe).
- TUI `apply_ui_event` consumes `SttError` and constructs the
  toast — newline-truncation + 6s TTL come from AC1's existing
  `SttErrorToast::new`.
- 2 new tests: happy-path (multi-line message → first line only,
  label contains `[class]` + `open doctor`) and missing-fields
  fallback (`unknown` / `(no message)` instead of silent drop).

## Diff summary

- 4 files modified (~110 LOC):
  - `crates/caco-daemon/src/ui_stream.rs` (enum variant + docs)
  - `crates/caco-daemon/src/audio.rs` (4 publish sites)
  - `crates/caco-tui/src/state/mod.rs` (apply_event arm)
  - `crates/caco-tui/src/state/tests.rs` (2 tests)
- Tests: 162 test-small + 2 new pass.

## Operator-takeaway

End-to-end voice-call UX now has a visible failure surface: an STT
hiccup on a live call shows up immediately as a top-of-TUI toast
labelled `⚠ [transcribe] <first line>  open doctor` instead of
silently failing in the daemon log. Pairs with bd-128ea6's
structured `stt_event_class=` log key — same classes, same
post-mortem story, two different surfaces.

Composes with bd-c16753 visual indicators (the toast lives next to
the status dots) and bd-128ea6's `caco stt doctor` (the
`open doctor` link in the toast points there).

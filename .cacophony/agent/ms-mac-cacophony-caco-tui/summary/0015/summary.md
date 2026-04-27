# Session summary — Local TUI TTS speed control

## Goal

Fix the TUI speech settings gap where TTS speed could be controlled from the daemon tab but not from the local TUI audio controls when the TTS daemon is not running.

## Bead(s)

- `bd-f39100` — cant control tts speed from tui except for daemon

## Before state

- Failing tests: none existing for the missing local speed row.
- Relevant metrics: not a performance change.
- Context: `SpeechState` already stored `tts_speed` from config and passed it to local TTS synthesis, and the daemon tab exposed `Daemon Speed`. The TUI Audio / flat speech popup rows did not expose `TTS Speed`, so changing local playback speed required daemon-backed controls or config edits.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: the speech popup now shows a `TTS Speed` row in TUI Audio / non-daemon mode. Activating it cycles local speed through `0.5`, `0.75`, `1.0`, `1.25`, `1.5`, and `2.0`, starting from provider default as `1.0`. When daemon-sync is explicitly configured, the local state still updates and the change is forwarded to the daemon control endpoint.

## Diff summary

- Commits: `587ab0723`
- Files touched: `crates/caco-tui/src/speech.rs`, `crates/caco-tui/src/views/speech_popup.rs`
- Tests: +2 regression tests / -0 / flipped existing popup row-count/index expectations for the new row
- Behavioural delta: local TUI speech playback speed can now be adjusted from the popup without a running TTS daemon.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui cycle_tts_speed --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui speech_popup --lib`

## Operator-takeaway

The top-right TUI speech popup now has parity for speed control: daemon users still have daemon speed controls, and local TUI playback users can change speed directly without editing config or starting the daemon.

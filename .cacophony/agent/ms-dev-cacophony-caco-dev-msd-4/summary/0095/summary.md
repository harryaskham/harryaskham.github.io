# Session summary — full Audio tools read-aloud label

## Goal

Make the TUI Audio tools read-aloud row use the same clear wording as the speech popup by replacing the abbreviated `Read Msgs` label with `Read Messages Aloud` and adding render coverage for both states.

## Bead(s)

- `bd-540647` — TUI Audio tools should use full Read Messages Aloud label

## Before state

- Failing tests: none observed for this bead.
- Relevant metrics: Audio tools rendered the TTS read-aloud setting as `Read Msgs`, while the speech popup rendered the same setting as `Read Messages Aloud`.
- Context: this followed the voice-rotation Audio tools polish and continued aligning the inspection view with speech popup terminology.

## After state

- Failing tests: none in validation.
- Relevant metrics: Audio tools now renders `Read Messages Aloud` and shows `ON` or `OFF`; render tests cover both states and assert the old `Read Msgs` abbreviation is absent.
- Context: the behavior remains read-only in Audio tools; this is a display/terminology parity fix.

## Diff summary

- Commit: `82c164e75` (`bd-540647: use full audio read-aloud label`)
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-tui audio_view_shows_read_messages_aloud --lib`; `cargo test -p caco-tui views::audio::tests --lib`; `cargo test-small`; `git diff --check`.
- Behavioural delta: operators now see the full `Read Messages Aloud` label in Audio tools with explicit ON/OFF state, matching the speech popup language.

## Operator-takeaway

This is a small but visible TUI consistency fix: the Audio tools panel now uses the same operator-facing wording as the speech popup, reducing abbreviation-driven ambiguity while keeping the existing state display intact.

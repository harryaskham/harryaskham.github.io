# Session summary — explicit TUI voice rotation row

## Goal

Make the TUI Audio tools view match the speech popup by showing voice rotation state as its own explicit row, rather than only appending an implicit suffix to the Voice row when rotation is active.

## Bead(s)

- `bd-b8688e` — TUI Audio tools should show voice rotation state explicitly

## Before state

- Failing tests: none observed for this bead.
- Relevant metrics: Audio tools rendered a Voice row and only appended `[rotation active]` plus a compatible-pool count when rotation was enabled.
- Context: when voice rotation was disabled, the Audio tools view had no explicit Voice Rotation row, while the speech popup already exposed a clear ON/OFF setting.

## After state

- Failing tests: none in validation.
- Relevant metrics: Audio tools now always renders `Voice Rotation` with `ON` or `OFF`; when enabled it preserves the compatible voice pool count, e.g. `(2 in pool)`.
- Context: the Voice row now stays focused on the selected voice, and the rotation state is visible as a separate read-only status row.

## Diff summary

- Commits: `b0c197f8d` (`bd-b8688e: show audio voice rotation row`), `4463dbd67` (`bd-b8688e: resolve audio rotation rebase overlap`)
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-tui audio_view_shows_voice_rotation --lib`; `cargo test -p caco-tui views::audio::tests --lib`; `cargo test-small`; `git diff --check`.
- Behavioural delta: the Audio tools TTS section now has explicit voice-rotation visibility for both disabled and enabled states, with render coverage for each.

## Operator-takeaway

This is a small parity/polish fix: operators can now read voice rotation state in Audio tools without knowing that the old active-only suffix existed, and the pool-count signal remains available when rotation is on.

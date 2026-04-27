# Session summary — Keep TTS daemon details visible in Audio tools

## Goal

Continue the TUI improvement loop by fixing a visibility issue found while testing Audio tools daemon diagnostics: the daemon detail rows could be clipped at normal terminal height.

## Bead(s)

- `bd-884b4d` — TUI Audio tools should keep TTS daemon details visible at 40 rows

## Before state

- Failing tests: no dedicated clipping regression existed.
- Relevant metrics: not a performance change.
- Context: the Audio tools `TTS Daemon` section rendered after Output Routing, Input Routing, and Per-Agent Audio. In a 120x40 render, the live daemon `Queue` row was clipped out, even though the section had just been expanded with richer diagnostics.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: the `TTS Daemon` section now renders immediately after the TTS settings section, before the long STT/routing/per-agent sections. The existing 40-row render helper now validates the live daemon details, including queue, without needing extra height.

## Diff summary

- Commits: `de23cf018`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: strengthened existing live-daemon render regression for 40-row height / -0 / flipped 0
- Behavioural delta: Audio tools section order changes so daemon diagnostics are visible earlier; rows and control semantics are otherwise preserved.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_live_tts_daemon_details --lib`

## Operator-takeaway

The expanded TTS daemon diagnostics are now useful in normal terminal sizes: operators can see queue and live status details without needing a taller-than-usual TUI.

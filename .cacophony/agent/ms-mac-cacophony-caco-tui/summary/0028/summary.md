# Session summary — Audio tools TTS daemon live-status reachability

## Goal

Continue the TUI improvement loop by making Audio tools distinguish TTS daemon process liveness from live-status API reachability.

## Bead(s)

- `bd-f6890c` — TUI Audio tools should show unreachable TTS daemon live status

## Before state

- Failing tests: none existing for the TTS daemon live-status unreachable display.
- Relevant metrics: not a performance change.
- Context: after adding rich live daemon details, Audio tools showed those details only when `tts_daemon_live_status.reachable` was true. If the daemon process was alive but the status poll was unreachable, the section still only showed the process status row.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: when the TTS daemon process is alive but the live-status snapshot is unreachable, Audio tools now renders a `Live Status: unreachable` row. Rich details still render when the live snapshot is reachable.

## Diff summary

- Commits: `fdb00be80`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +1 regression test / -0 / flipped 0
- Behavioural delta: no daemon-control semantics changed; Audio tools now surfaces status-poll reachability as a separate diagnostic from process liveness.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_unreachable_tts_daemon_live_status --lib`

## Operator-takeaway

If the TTS daemon process is running but its status API is unreachable, Audio tools now says so explicitly instead of leaving operators to infer why the live daemon details are missing.

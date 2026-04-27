# Session summary — TTS daemon last-poll age in Audio tools

## Goal

Resume the TUI improvement loop after the reintegration safety hold cleared by landing the held Audio tools enhancement that makes TTS daemon live-status freshness visible.

## Bead(s)

- `bd-c6975c` — TUI Audio tools should show TTS daemon last poll age

## Before state

- Failing tests: none known for this path.
- Relevant metrics: not a performance change.
- Context: Audio tools showed rich TTS daemon details when the live status snapshot was reachable, but it did not indicate when that snapshot was last polled. The change had been held locally while `bd-95cda5` reintegration safety was active.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: when `tts_daemon_live_status.last_poll` is present, Audio tools now renders a read-only `Last Poll` row with compact age text such as `2m ago`, next to the other live daemon details.

## Diff summary

- Commits: `ba86f3ff4` (rebased held work; original held commit was `84e8e8171` before rebase)
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: updated 1 regression test / -0 / flipped 0
- Behavioural delta: no daemon-control semantics changed; Audio tools now surfaces the freshness of the displayed daemon live-status snapshot.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_shows_live_tts_daemon_details --lib`

## Operator-takeaway

Audio tools now tells operators how fresh the TTS daemon live-status data is, making it easier to distinguish current daemon state from stale-but-still-rendered diagnostics.

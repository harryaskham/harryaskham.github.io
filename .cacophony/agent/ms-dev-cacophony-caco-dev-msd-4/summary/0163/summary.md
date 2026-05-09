# Session summary — Audio transcription 502 stderr replay guard

## Goal

Stop nonfatal `POST /api/v1/audio/transcription -> 502` diagnostics from continuing to populate `daemon-crash.log` after `bd-89c1aa`, especially when stale `ERROR [daemon:http]` tails with `fatal:false` are replayed through startup previous-stderr banners.

## Bead(s)

- `bd-5cdbb4` — Audio transcription 502 recurs after bd-89c1aa close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor reported `daemon.log` at 6,349,472 bytes with 78 window lines and `audio_502=3`; `daemon-crash.log` at 1,152,074 bytes with 44 window lines and `audio_502=3` in the 2026-05-09T02:21:29Z to 2026-05-09T02:36:29Z sweep.
- Context: `bd-89c1aa` made fresh audio transcription 502 records warning severity, but deployed crash-log tails still contained older `ERROR [daemon:http] ... severity:error fatal:false` records that could be mirrored again by previous-stderr replay.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed after rebase: `tj-0425d951` for the focused daemon stderr-mirror test and `bj-5d369ffb` for `cargo check -p caco-daemon --lib`.
- Context: the daemon stderr mirror guard now recognizes nonfatal audio transcription 502 diagnostics by component/path/status/fatal marker, suppressing both direct stale records and startup previous-stderr replay wrappers while leaving transcription 500s crash-visible.

## Diff summary

- Commits: `c83c68a956`.
- Files touched: `crates/caco-daemon/src/logging.rs`.
- Tests: +1 daemon unit test / -0 / flipped 0.
- Behavioural delta: nonfatal audio transcription 502 route diagnostics remain available through daemon/feed logging but no longer re-enter supervised stderr/crash-log when stale severity-error lines are replayed.

## Operator-takeaway

The post-`bd-89c1aa` recurrence was a replay of older severity-error HTTP diagnostics, not a new daemon crash. The guard now treats fatal=false transcription 502s as non-crash stderr content even if the line predates the warning-severity fix.

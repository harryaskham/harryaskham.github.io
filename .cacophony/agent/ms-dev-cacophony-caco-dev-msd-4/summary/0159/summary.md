# Session summary — Replayed idle-advisory stderr tails

## Goal

Stop persistent idle-advisory text from being re-mirrored into `daemon-crash.log` when it appears inside startup previous-stderr banners or other non-`agent` log messages, after the source-side guard for direct agent advisories had already landed.

## Bead(s)

- `bd-1113b7` — caco-aks idle advisory still writes to daemon-crash.log after bd-d4bcd3 close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor reported `daemon-crash.log` at 684,883 bytes with 22 timestamped lines in the 2026-05-09T01:51:13Z to 2026-05-09T02:06:13Z sweep; 7 were idle-advisory lines.
- Context: `EventLogger::should_mirror_stderr_line` suppressed direct `component=agent` advisory messages, but a stale advisory tail could be embedded in a supervision/startup previous-stderr banner and mirrored under a different component.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed: `tj-28b4a527`, `tj-574627b4`, and `bj-37a5f9e4`.
- Context: the stderr-mirror guard now recognizes the warning-only persistent idle-advisory text shape independent of component, including previous-stderr banner bodies, while preserving ordinary non-advisory warning mirroring.

## Diff summary

- Commits: `cb6305498d`.
- Files touched: `crates/caco-daemon/src/logging.rs`.
- Tests: updated 1 daemon unit test / -0 / flipped 0.
- Behavioural delta: advisory-only persistent idleness remains in daemon/feed diagnostics but is not mirrored back to stderr/crash-log when replayed through supervision banner text.

## Operator-takeaway

The recurring caco-aks idle-advisory crash-log noise was likely a replay path, not the original source path. This fix makes the guard content-based so stale advisory tails do not keep recontaminating `daemon-crash.log` under a different component.

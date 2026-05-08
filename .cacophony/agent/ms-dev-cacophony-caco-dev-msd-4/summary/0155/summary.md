# Session summary — caco-aks idle advisory write-path guard

## Goal

Stop the repeated caco-aks persistent-idle advisory noise from contaminating `daemon-crash.log` after the bd-ff00ca classifier fix by covering the actual stderr-router write path, not just the classifier return value.

## Bead(s)

- `bd-5bceb1` — caco-aks idle advisory still writes to daemon-crash.log after bd-ff00ca close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: log-monitor evidence showed 18 timestamped WARN idle-advisory lines in the 2026-05-08T23:26:43Z → 2026-05-08T23:41:43Z crash-log sweep, including caco-aks examples at 23:34:03.437Z and 23:41:22.056Z.
- Context: bd-ff00ca already taught the classifier that those line shapes are non-crash diagnostics, but the recurrence warranted a regression at the file-append layer that actually decides whether `daemon-crash.log` grows.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar validation passed for the new bd-5bceb1 test and the full daemon stderr-router subset.
- Context: timestamped EventLogger idle advisory lines are dropped as already logged; replayed/non-timestamped advisory lines are preserved in `daemon.log`; neither path appends to `daemon-crash.log`.

## Diff summary

- Commits: b7ab092d4a
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_post_ff00ca_write_path_keeps_idle_advisory_out_of_crash_log_bd_5bceb1`.
- Behavioural delta: extracted the per-line stderr router append logic into `route_daemon_stderr_line_to_files` so the exact file-write behavior is unit-testable, and covered the observed post-bd-ff00ca caco-aks idle-advisory examples.
- Validation: `git diff --check`; `tj-613a2301` passed `cargo test -p caco-sidecar bd_5bceb1 -- --nocapture`; `tj-ed414756` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`; post-rebase `tj-431880b6` passed the same router subset.

## Operator-takeaway

This turns the recurring idle-advisory crash-log regression into a write-path invariant: future timestamped caco-aks advisory lines should not grow `daemon-crash.log`, while replayed advisory context still remains visible in the normal daemon log.

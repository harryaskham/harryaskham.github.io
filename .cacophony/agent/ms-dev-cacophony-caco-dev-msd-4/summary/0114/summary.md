# Session summary — Warning-only reconciliation diagnostics stay out of crash log

## Goal

Fix `bd-bfdb1f`, where startup reconciliation’s warning-only diagnostics for already-terminal failed agents were useful structured signal but were also mirrored to daemon stderr, causing supervised nodes to accumulate non-crash warning floods in `daemon-crash.log`.

## Bead(s)

- `bd-bfdb1f` — `daemon-crash.log receives steady warning-only startup reconciliation diagnostics`

## Before state

- Failing tests: no local failing test; live log-monitor evidence showed Helsinki healthy but still accumulating `WARN [agent_lifecycle] ... startup reconciliation observed terminal failed agent ... warning-only diagnostic` lines in `daemon-crash.log`.
- Relevant metrics: the user relayed that the latest Helsinki sweep improved from 75 to 52 warning-only crash-log lines, but the class still recurred.
- Context: Structured warning records were already persisted as feed `log_error` events and daemon.log lines via `persist_structured_log_error`, but the shared logger mirrored warn-level lines to stderr by default.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: warning-level, non-fatal structured log-error records now write to daemon.log/feed without stderr mirroring; error/critical/fatal records still use the normal stderr-mirrored path. The startup reconciliation aggregate note was also moved from raw `eprintln!` to the same daemon.log-only warning path.
- Context: Startup reconciliation diagnostics remain queryable in structured logs/feed and daemon.log, but should no longer pollute `daemon-crash.log` once this build is deployed.

## Diff summary

- Commits: implementation commit `bd-bfdb1f: keep warning-only diagnostics out of crash log` plus this summary commit.
- Files touched: `crates/caco-daemon/src/logging.rs`, `crates/caco-daemon/src/lib.rs`, `SPEC.md`.
- Tests: extended `startup_reconciliation_reports_per_agent_failures` to assert warning-only reconciliation diagnostics remain in daemon.log/feed after stderr suppression.
- Behavioural delta: `EventLogger` now supports per-call no-stderr logging; `persist_structured_log_error` uses it for non-fatal warnings, while preserving stderr mirroring for actionable errors and fatal records.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-daemon startup_reconciliation -- --nocapture`.

## Operator-takeaway

This is a source-level fix for the warning-only crash-log noise class: once deployed, startup reconciliation warnings should remain visible in daemon.log/feed without being ingested as daemon crash stderr.

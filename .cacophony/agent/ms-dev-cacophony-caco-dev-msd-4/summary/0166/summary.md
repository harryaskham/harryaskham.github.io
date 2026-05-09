# Session summary — Make log-monitor suppress already-covered crash-log recurrences

## Goal

Stop the burn-down loop from filing another implementation bead every time ms-mac's live crash log still contains a known non-crash diagnostic shape that current landed code already classifies correctly. This slice focused on `bd-5b5de1`, another persistent-idle-advisory recurrence after the earlier crash-log hygiene fixes landed.

## Bead(s)

- `bd-5b5de1` — caco-aks idle advisory still writes to daemon-crash.log after bd-3fbacf close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor reported `idle_advisory=14` in `daemon-crash.log` for the 2026-05-09T03:51:20Z to 2026-05-09T04:06:20Z sweep after `bd-3fbacf` closed.
- Context: current daemon code already suppresses and prunes persistent idle advisories, watchdog diagnostics, and nonfatal transcription 502s, but the log-monitor profile's generic already-covered recurrence rule was not concrete enough to stop this per-sweep duplicate filing pattern.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed after rebase: `tj-16171821` for `log_monitor_persistent_stack_excludes_worker_lifecycle_mixins` and `bj-ae0b40ac` for `cargo check -p caco-profile --lib`. A first mistaken filter job `tj-c2e747c8` ran zero tests and was superseded by the correct targeted test.
- Context: the log-monitor profile now explicitly names the already-covered crash-log hygiene examples: `persistent_idle_advisory` / advisory-only persistent-agent lines, `[tts-watchdog]` stale-process respawn lines, `diagnostic=pid-only-watchdog`, and nonfatal `POST /api/v1/audio/transcription -> 502` with `"fatal":false`. The caco-profile regression now asserts those examples stay in the checked-in log-monitor prompt.

## Diff summary

- Commits: `3a62b4082a`.
- Files touched: `.cacophony/profiles/log-monitor.md`, `crates/caco-profile/src/lib.rs`.
- Tests: +0 new tests / -0 / strengthened 1 existing profile regression.
- Behavioural delta: future log-monitor materializations have explicit instructions not to file per-sweep recurrence beads for these known-covered crash-log shapes; they should aggregate the signatures or file at most one rollout/deployment-convergence tracker instead.

## Operator-takeaway

The repeated idle-advisory beads were no longer finding new daemon classifier gaps; they were duplicate live-log recurrence filings. This change makes the log-monitor prompt name the known-covered shapes so future sweeps stop feeding the worker queue with identical implementation beads while rollout catches up.

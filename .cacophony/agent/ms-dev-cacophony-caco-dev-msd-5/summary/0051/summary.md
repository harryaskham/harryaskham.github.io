# Session summary — Persistent idle advisories stay out of crash log

## Goal

Stop routine persistent-agent idle advisories from recurring in `daemon-crash.log` after the earlier closeout for `bd-1b5014`. The goal was to keep the warning visible as durable operator diagnostics while reserving the crash log for actual fatal/death evidence.

## Bead(s)

- `bd-6dacb0` — caco-aks idle advisory still writes to daemon-crash.log after bd-1b5014 close

## Before state

- Log-monitor evidence showed 13 timestamped `persistent_idle_advisory` WARN lines mirrored into both `daemon.log` and `daemon-crash.log` over a 15-minute sweep.
- Example recurrence: `persistent agent ms-mac-cacophony-caco-aks idle for 1810s (threshold 1800s) — advisory only; live runtime not auto-restarted without positive death evidence`.
- `caco status` and `caco service status` were healthy; the advisory did not represent a panic, OOM, or fatal daemon condition.

## After state

- `auto_restart_idle_persistent_agents` now emits the advisory through `EventLogger::log_without_stderr_mirror`, preserving daemon.log/feed visibility without writing the structured warning to daemon stderr.
- The redundant explicit `eprintln!` advisory was removed, closing the direct stderr bypass that could still feed `daemon-crash.log`.
- The existing advisory-only regression test now asserts the handler uses non-stderr mirrored logging and does not contain an `eprintln!` bypass.
- Focused validation passed: `tj-4ff14a84` ran `cargo test -p caco-daemon persistent_idle_handler_is_advisory_only_bd_db327a -- --nocapture` successfully.

## Diff summary

- Commits: code/content commit to be listed in the reintegration receipt; current branch commit contains the `bd-6dacb0` changes.
- Files touched: `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: strengthened the existing persistent idle advisory unit/source regression.
- Behavioural delta: persistent idle advisories remain warning-level durable diagnostics but no longer poison `daemon-crash.log` unless a separate fatal condition is actually present.

## Operator-takeaway

The idle advisory still tells operators that a persistent agent has been quiet past its threshold, but it no longer masquerades as crash evidence. `daemon-crash.log` should stay focused on real daemon/runtime failures.

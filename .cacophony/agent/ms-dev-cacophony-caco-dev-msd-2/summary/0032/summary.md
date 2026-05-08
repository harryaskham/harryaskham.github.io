# Session summary — Rate-limit persistent idle advisories

## Goal

Stop live persistent-agent idle advisories from repeatedly growing `daemon-crash.log` while preserving the useful daemon log/feed signal that an agent has exceeded its idle threshold.

## Bead(s)

- `bd-1b5014` — Persistent caco-aks idle advisory spams daemon-crash.log while agent is still running

## Before state

- Failing tests: none known for this focused path.
- Relevant metrics: log-monitor evidence showed `daemon-crash.log` grew by 43,178 bytes in 15 minutes with 24 repeated persistent idle advisory WARN lines for a running `caco-aks` agent.
- Context: `auto_restart_idle_persistent_agents` logged every eligible candidate through the normal warning path and also wrote an explicit `eprintln!`, so supervised deployments mirrored routine advisory text into `daemon-crash.log`.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: queued focused validation passed after rebase in `tj-c0da5f32` (`RUST_MIN_STACK=33554432 cargo test -p caco-daemon persistent_idle_advisory_is_rate_limited_and_not_stderr_mirrored_bd_1b5014 -- --nocapture`). Earlier pre-rebase focused validation also passed in `tj-ea6caa7e`.
- Context: persistent idle advisories are now per-agent rate-limited to one log/feed emission per 30 minutes and use the no-stderr-mirror logging path.

## Diff summary

- Commits: `cb4b3d872a`
- Files touched: `crates/caco-daemon/src/lib.rs`, `SPEC.md`
- Tests: +1 focused regression covering rate-limit helper behavior and no stderr mirror in the handler.
- Behavioural delta: `persistent_idle_advisory` remains visible in daemon.log/feed metadata with `persistent_idle_advisory_log_interval_secs` and `stderr_mirror_suppressed`, but warning-only repeats no longer write to daemon stderr / `daemon-crash.log`.

## Operator-takeaway

Routine live-agent idle advisories should no longer obscure real crash evidence. Operators still get a bounded advisory signal, while `daemon-crash.log` stays reserved for fatal daemon stderr.

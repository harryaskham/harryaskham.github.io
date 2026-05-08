# Session summary — caco-aks idle advisory crash-log routing

## Goal

Keep post-bd-be6e7f caco-aks persistent-idle advisory lines out of `daemon-crash.log` while preserving real panic/fatal stderr routing.

## Bead(s)

- `bd-ff00ca` — caco-aks idle advisory still writes to daemon-crash.log after bd-be6e7f close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: log-monitor evidence showed 21 timestamped `persistent_idle_advisory` WARN lines mirrored into both `daemon.log` and `daemon-crash.log` between 2026-05-08T21:26:36Z and 2026-05-08T21:41:36Z.
- Context: earlier fixes already covered several idle-advisory spellings; this bead captured another observed recurrence for `ms-mac-cacophony-caco-aks` after bd-be6e7f.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar stderr-router validation passed.
- Context: sidecar stderr routing now recognizes the structural persistent-agent idle advisory shape even if a replayed/legacy line lacks the exact prior `persistent_idle_advisory` marker or timestamp prefix, while timestamped EventLogger WARN lines remain suppressed as already logged.

## Diff summary

- Commits: `bcd9944143`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_post_be6e7f_idle_advisory_shapes_bd_ff00ca`.
- Behavioural delta: observed post-bd-be6e7f caco-aks idle advisory examples route to `AlreadyLogged`, and legacy/replayed variants route to `DaemonLog` instead of `CrashLog`.
- Validation: `git diff --check`; `tj-7118e304` passed `cargo test -p caco-sidecar bd_ff00ca -- --nocapture`; `tj-8bd0e489` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`; `tj-b2d6b3b8` passed the same `daemon_stderr_router` subset after rebase.

## Operator-takeaway

This is another defensive crash-log hygiene guard: routine persistent-agent idle advisories should no longer obscure real daemon crash evidence, even when the line shape is replayed without the structured/timestamped wrapper.

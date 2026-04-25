# Session summary — zombie pid-only services are stopped

## Goal

Keep ms-mac healthy by addressing `bd-0c272e`, where `caco-web` intermittently appeared as PID-alive but health-unresponsive or stopped while the daemon and TTS remained running.

## Bead(s)

- `bd-0c272e` — ms-mac caco-web process stops while daemon and TTS stay running

## Before state

- Failing tests: none known for this scope.
- Relevant metrics: live ms-mac inspection showed `caco web status` with `process_alive=true` for PID `70508`, but `/health` failed and `ps` showed that PID as `<defunct>` with parent `caco-daemon`. `caco status` simultaneously reported the web service as running because pid-only liveness used `kill(pid, 0)`.
- Context: historical daemon logs showed repeated `caco-web PID ... alive but port unresponsive` kills/respawns and `AddrInUse` retries, matching a zombie/stale pid-only service being treated as alive.

## After state

- Failing tests: none in scoped validation.
- Relevant metrics: `cargo test -p caco-sidecar ps_stat_zombie_parser_detects_zombies` passed; `cargo test -p caco-sidecar is_process_alive_for_current` passed; `cargo check -p caco-sidecar` passed.
- Context: the shared process-liveness helper now treats zombie processes as not alive, so pid-only services such as `caco-web` should not be reported healthy or skipped by convergence solely because a defunct PID still answers signal 0.

## Diff summary

- Commits: `1e6995674` (zombie PID liveness change; this summary is committed as sibling session-recording commits)
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: +1 parser regression for zombie `ps` state; existing current-process liveness test retained.
- Behavioural delta: stale/defunct service PIDs are cleaned up and reconverged instead of masquerading as live pid-only services.

## Operator-takeaway

The ms-mac web instability had a concrete liveness bug: a zombie `caco web` process could look alive to `kill(pid, 0)`. The lifecycle layer now treats that state as stopped so health and reconvergence match reality.

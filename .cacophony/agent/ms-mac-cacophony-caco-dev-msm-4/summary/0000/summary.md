# bd-2324c2 — reap orphan caco-web pid before respawning the in-process dashboard

## Goal
Stop the recurring "Web dashboard exited: Address already in use
(os error 48)" + dashboard-down-for-2.5min cascade that fires on
every daemon restart.

## Bead(s)
- bd-2324c2 (P2 bug, log-monitor) — `caco-web port 11180 collision
  recurs on EVERY daemon restart — bd-3af67d closed prematurely`.
  Multi-occurrence successor to bd-3af67d (closed after one
  observation).  Pairs with bd-5f8223 (tts-watchdog stale-listener
  reaper), bd-1f3d7c (sidecar pid_only caco-web), bd-b2b40e
  (in-process axum task), bd-3af67d (bind_with_retry backoff).

## Before state
- Two competing caco-web launchers were live in the tree:
  1. `crates/caco-sidecar/src/lifecycle.rs:512` (bd-1f3d7c) — sidecar
     pid_only spawn of `caco web --port 11180 --bind ...`.
  2. `crates/caco-daemon/src/lib.rs:7702` (bd-b2b40e) — in-process
     axum task on the daemon itself bound to the same port.
- On daemon restart the old in-process task released the port (its
  process is gone) but the standalone sidecar caco-web survived —
  no one stopped it during the shutdown sequence. The new daemon
  then tried to bind 11180 in-process and hit `EADDRINUSE`.
- bd-3af67d's `bind_with_retry` inside caco-web (1+2+4+8+15s
  backoff, ~30s total) gave up well before the orphan was reaped.
  tts-watchdog (bd-5f8223) eventually noticed the orphan's listener
  was unhealthy and killed it ~2.5min later, then respawned. Net
  effect: dashboard dark on every restart.
- log-monitor confirmed the pattern across 3 daemon restarts on
  ms-mac in one day (10:28, 14:02, ~14:54 UTC).

## After state
- Before launching the in-process dashboard task, the daemon now
  reads `paths.web_pid` (the canonical sidecar pid file at
  `<dir>/daemon/web.pid`). If that pid is alive, `reap_orphan_caco_web`
  sends SIGTERM and polls for exit (up to 2s), then SIGKILL fallback
  (up to 500ms). After SIGKILL the kernel guarantees the process is
  dead; the helper returns `true` even if the pid lingers as a zombie
  pending parent reap, since the listener has been released.
- On success the daemon emits a single `INFO [caco-web] reaped
  orphan caco-web pid=<n> on port <p> before respawn (bd-2324c2)`
  line, then proceeds to `bind_with_retry`.
- `bind_with_retry` inside caco-web remains as the safety net for
  cases the pid file lookup misses (absent file, exotic launcher,
  recycled pid).
- Cross-platform: uses `libc::kill(pid, 0|SIGTERM|SIGKILL)` directly,
  works on macOS, Linux, BSDs — no extra crate.

## Diff summary
- `crates/caco-daemon/src/lib.rs` (+114/-0):
  - New `reap_orphan_caco_web(pid) -> bool` async helper next to
    `spawn_background_task`.
  - In the in-process web-dashboard launch site (line ~7702-region):
    read `state.paths.web_pid`, parse, call the reaper before
    `spawn_background_task("web dashboard", ...)`.
  - Two unit tests in `caco_daemon::tests`:
    - `reap_orphan_caco_web_no_op_when_pid_absent` — phantom pid
      `2_000_000_000` returns `true` immediately.
    - `reap_orphan_caco_web_terminates_live_process` — spawns a
      real `sleep 30` child, reaps it, asserts the helper returns
      `true`.

## Operator-takeaway
- After this rolls out and the next daemon restart cycles the
  binary, the dashboard should come back up within ~100ms instead
  of going dark for ~2.5min.
- Risk envelope: SIGTERM to a recycled pid is theoretically possible
  but the recipient is same-host same-uid and the existing
  tts-watchdog reaper (bd-5f8223) already takes the same risk; no
  net escalation.
- Followup (out of scope here): collapse the dual launcher entirely
  — only one of bd-1f3d7c (sidecar) and bd-b2b40e (in-process)
  should exist. That's a multi-day refactor; this bead is the
  cheap defensive fix that closes the operator-visible window.

## Tests
- `cargo test -p caco-daemon --lib reap_orphan` — 2/2 passed.
- `cargo build -p caco-daemon` — clean.
- `cargo clippy -p caco-daemon --all-targets -- -D warnings` — clean.

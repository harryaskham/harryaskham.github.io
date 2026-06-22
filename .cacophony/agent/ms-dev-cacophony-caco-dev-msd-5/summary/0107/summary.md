# Session summary — durable daemon stderr capture across supervisor re-exec/adopt (bd-7e679d)

## Goal

After a v1.2.1326 restart the daemon's steady-state `INFO [stderr]` stopped being
captured into daemon.log (5 startup lines then 0; not in crash.log either),
blinding daemon.log-based log-monitoring of model_discovery / cluster-pull /
bg-refresh fleet-wide.

## Root cause

The `[stderr]` router was attached on ONLY the SPAWN path
(`spawn_daemon_stderr_router`, caco-sidecar/lifecycle.rs); the ADOPT path
(bd-95352b — "adopted existing listener owner instead of replacing it") attached
no router. caco-daemon's stderr was a PIPE to the supervisor process; a
`caco update --restart` re-exec/restart killed the router thread with the old
supervisor + closed the pipe read end, then the new supervisor ADOPTED the
running daemon (no router) → daemon kept eprintln!-ing to a dead pipe → 0
captured lines. (v1.2.1327's restart incidentally restored capture by landing on
a SPAWN rather than an adopt, but the adopt-path gap remains latent in current
code and will recur on the next re-exec+adopt.)

## After state (durable robustness fix)

- caco-daemon stderr now writes to a PERSISTENT raw file
  (`caco-daemon-stderr-raw.log`, beside daemon-crash.log) instead of a pipe; the
  raw file + the daemon's inherited stderr fd survive supervisor re-exec.
- A file-tail router (`route_daemon_stderr_tail_file` via a blocking
  `TailingFileReader`) REUSES the existing `route_daemon_stderr_stream` +
  `daemon_stderr_route_for_line`, so crash-vs-daemon classification is identical
  to the old pipe router (panic/backtrace → crash.log; routine → daemon.log).
- `ensure_daemon_stderr_file_router` attaches the tail router on BOTH spawn
  (from_start, fresh truncated file) AND adopt (from EOF), idempotently
  (per-process guard, no double-routing).
- Bonus (ctrl-flagged): "ui snapshot: store lock contended" degraded-read
  diagnostics now route to daemon.log, not crash.log.

## Diff summary

- File: `crates/caco-sidecar/src/lifecycle.rs` (file-tail router + spawn/adopt
  wiring + classification fix; removed the now-unused pipe `spawn_daemon_stderr_router`).
- Tests: `store_lock_contended_routes_to_daemon_log_bd_7e679d`,
  `daemon_stderr_raw_path_is_beside_crash_log_bd_7e679d`,
  `file_router_splits_crash_vs_daemon_from_a_file_bd_7e679d` (KEY: panic still →
  crash.log after the pipe→file rewrite). 22 stderr-router lib tests pass.
- Final landed squash SHA per the reintegration receipt.

## Operator-takeaway

Daemon stderr capture (and crash/panic routing) now survives supervisor
re-exec/restart + the bd-95352b adopt path, so a future restart can't silently
blind daemon.log of steady-state stderr again. Reviewed pre-land by ctrl
(fleet-critical observability path).

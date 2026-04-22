# Session summary — bd-ca208d: caco tmux cleanup catches live-owner zero-session sockets; --dry-run

## Goal

`caco tmux cleanup` was a no-op against real-world stale
sockets — test-user observed 6 caco-test-* sockets aged
47-50h with `session_count=0 has_clients=false
owner_alive=true` surviving repeat cleanup runs.  Tmux
servers self-host as daemons and keep their PID alive
long after the last session/client exits, so the legacy
"prune only when owner_alive=false" guard rarely fired.

## Bead(s)

- `bd-ca208d` — P3 bug, test-user-hel filed.

## Before state

- `prune_stale_test_tmux_servers` skipped any socket
  whose `owner_pid` was alive — even if it had zero
  sessions and no clients.
- No `--dry-run` flag, so operators couldn't preview
  what cleanup would do.
- Help text didn't hint at the live-owner blind spot.

```
$ caco tmux status --json | jq '...'  # 6 sockets, age 47-50h
$ caco tmux cleanup --json
{ "ok": true, "pruned": [], "pruned_count": 0 }
```

## After state

```
$ caco tmux cleanup --dry-run
caco tmux cleanup --dry-run — 0 candidate(s)
  No prunable caco-test-* tmux servers found.

$ caco tmux cleanup --help
Arguments:
  --dry-run   List sockets that would be pruned without killing them (bd-ca208d).
```

Live behaviour change: when `owner_alive` is true but
`has_clients=false`, the pruner now runs `tmux -L
<sock> list-sessions -F #{session_name}` and prunes if
the count is 0.  All other guards preserved (no-clients
check still gates kill; threshold mtime check
unchanged; own-socket guard unchanged).

## Diff summary

- 2 files touched, +85 / −10:
  - `crates/caco-daemon/src/agent/health.rs`:
    `prune_stale_test_tmux_servers` reordered + augmented
    with the session-count probe in the live-owner
    branch.
  - `crates/caco-cli/src/lib.rs`:
    - new `TMUX_CLEANUP_ARGS` (`--dry-run`).
    - `TMUX_SUBCOMMANDS` cleanup-leaf converted from
      `mcp_leaf` shorthand to full `CommandSpec` with
      args + agent_safe + idempotent.
    - `dispatch_tmux_cleanup` adds the dry-run branch:
      reuses `list_test_tmux_sockets` + the same
      `(!has_clients) && (!owner_alive || session_count == 0)`
      predicate the pruner uses; renders text and JSON.

## Verification

- `cargo build --bin caco`: clean.
- `caco tmux cleanup --dry-run` and `--help`: correct
  output with the new flag listed.
- No live test-sockets on winmini today, so the
  live-owner branch wasn't exercised end-to-end here.
  Daemon helpers are pure, the predicate matches the
  one in `dispatch_tmux_status` rendering.

## Operator-takeaway

Pattern: when a "self-hosting daemon process" is the
target of a cleanup heuristic, owner_alive is rarely
the right signal.  Drill down to functional state
(session_count, attached clients, idle time) to decide
liveness honestly. The legacy code structure
("if alive: skip") was inverted; the new structure
checks each kill-condition in series.

`--dry-run` belongs on every destructive operator
command — the legacy `caco tmux cleanup` did this
silently, which test-user only caught via repeated
status comparison. File a follow-up if other destructive
caco subcommands lack `--dry-run` (saw at least
`caco prune run --dry-run` exists; this is the model to
mirror).

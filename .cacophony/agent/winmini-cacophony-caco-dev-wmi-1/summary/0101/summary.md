# Session summary — bd-7827c0: caco sidecar status gains --service filter (parity with start/stop/serve; bd-3656ce sibling)

## Goal

test-user-hel found that `caco sidecar status`
silently swallowed `--service` (warning fired but
the filter was ignored — operator got the full
service list). The other three sidecar
subcommands (start/stop/serve) accept --service
with consistent semantics. Status was the natural
read-only surface for asking about ONE service'
health, but lacked the flag.

Fix: add SIDECAR_STATUS_ARGS, upgrade the status
CommandSpec from mcp_leaf to a full spec, plumb
`--service` through dispatcher, validate against
configured services list (mirrors bd-3656ce on
start/stop), filter the result set.

## Bead(s)

- `bd-7827c0` — test-user-filed. Closed.

## Before state

```
$ caco sidecar status --service caco-daemon
warning: bd-b76723: caco sidecar status received
unrecognised flag(s): --service. These were ignored
sidecar status for node: helsinki
  ✓ caco-daemon desired=Running ...
  ✓ caco-tts-daemon desired=Running ...
                          # ✗ --service ignored
```

## After state

```
$ caco sidecar status --service caco-daemon
sidecar status for node: winmini
  ✓ caco-daemon desired=Running actual=running ...

$ caco sidecar status --service bogus
error: unknown service 'bogus' for node 'winmini'.
Configured: caco-daemon (or omit --service for all)

$ caco sidecar status --json --service caco-daemon
[filtered JSON envelope]

$ caco sidecar status --help
  --service   Service family to query status for (default: all).
```

## Diff summary

- 1 file touched, +51 / −5:
  - `crates/caco-cli/src/lib.rs`:
    - `SIDECAR_STATUS_ARGS` const (mirrors
      SIDECAR_START_ARGS shape).
    - `SIDECAR_SUBCOMMANDS` `status` upgraded from
      `mcp_leaf` to full `CommandSpec` so the
      arg surfaces in --help and --json help and
      the warning suppression works correctly.
    - Dispatcher: pulls `--service` from flags,
      forwards to dispatch_sidecar_status.
    - `dispatch_sidecar_status` signature changed
      to accept `service_filter: Option<&str>`;
      validates against
      `LifecycleManager::configured_services()`
      BEFORE the async status() call (early
      error); applies the filter as `retain` on
      the result vec.

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- `cargo clippy -p caco-cli`: clean.
- 5 cases verified live:
  - bare `sidecar status` unchanged.
  - `--service caco-daemon` returns just that
    service.
  - `--service bogus` rejected with enumerated
    configured list.
  - `--json --service ...` returns filtered
    envelope.
  - `--help` shows the new flag.

## Operator-takeaway

Cluster status (sibling of bd-3656ce family —
sidecar dispatchers all take --service): now
consistent across start/stop/serve/status. Cluster
status (silent-unknown-flag family — bd-b76723
warning visible to ops, ignored in agent context):
one fewer instance.

The recurring CLI honesty pattern — when 3 of 4
related subcommands accept a flag, the 4th
absence is operator-confusing more than
dangerous, but compounds quickly when scripts
need it. Cheap, mechanical fix at the dispatcher
layer; the validate-against-configured-list
sub-pattern (bd-3656ce) generalises naturally.

The mcp_leaf -> full CommandSpec upgrade is the
mechanism: mcp_leaf is a no-args shorthand; once
a subcommand grows args, the full spec form is
required so help and discoverability work
correctly.

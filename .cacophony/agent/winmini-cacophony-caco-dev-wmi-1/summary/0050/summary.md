# Session summary — bd-a403a1: node show error message honesty

## Goal

Fix `caco node show --node <bogus>` error so it stops
shouting "daemon unreachable" when the daemon is actually
healthy and the user simply typo'd a node name.

## Bead(s)

- `bd-a403a1` — P3 bug, test-user-hel filed papercut.

## Before state

- `caco node show --node nonexistent` returned:
  `error: node 'nonexistent' is not configured (daemon
   unreachable, cannot resolve dynamic nodes)`
- The parenthetical claimed daemon outage even when
  `caco node list` / `caco doctor` worked fine in the
  same shell.
- Same misleading text for `--node ''`.
- Source: lib.rs:52600, the daemon-fallback branch had
  no health probe — it just rendered the same error
  whether the daemon GET returned 404 or transport-error.

## After state

- The fallback branch now does a 2s `/api/v1/health`
  probe (same pattern as bootstrap dev start_daemon at
  lib.rs:50189). Two distinct messages:
  - daemon alive: "node 'X' is not configured (no static
    or dynamic match; daemon is reachable — check spelling
    or run 'caco node list')"
  - daemon dead: "node 'X' is not configured and daemon is
    unreachable, so dynamic nodes cannot be resolved"
- User now sees the real cause and an actionable next
  step ("check spelling or run 'caco node list'").

## Diff summary

- 1 file touched, +30 / −5:
  - `crates/caco-cli/src/lib.rs`: the bd-a403a1 block
    in `dispatch_node_show`'s daemon-fallback branch.

## Verification

- `cargo build -p caco-cli`: clean.
- Pattern verified to match the existing
  `dispatch_bootstrap_dev_start_daemon` health-probe
  helper (bd-6a50ec slice 1).

## Operator-takeaway

Family with bd-513fc8 / bd-eb84c8 / bd-126b99 (CLI
honesty pass): error messages should report the actual
state, not the worst case it could imagine. Cheap UX win
that prevents test-user / fresh-operator confusion.

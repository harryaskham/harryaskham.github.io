# Session 0013 — bd-1b713a (caco fleet snapshot)

## Goal

First slice of `caco fleet snapshot`: a single read-only orchestrator
that captures cluster state to a JSON envelope for postmortem analysis.

Out of scope (deferred): per-node disk/load (needs SSH or new
node-side endpoint), DB row counts (no endpoint), `--replay` mode.

## Bead(s)

- **bd-1b713a** — primary; new `caco fleet snapshot` subcommand.
- **bd-d46eb7** — filed (P2 follow-up to bd-7ef076): caco-tui has 85
  missing-field initializer errors from msd-1's tmux_history_* daemon
  snapshot landing partially. Too large to inline-absorb here. Daemon
  side fixture fix included in this commit so caco-daemon compiles.

## Before state

HEAD 23f797aa. No `caco fleet` branch existed. caco-tui broken-on-main.

## After state

- New `FLEET_SUBCOMMANDS` + `FLEET_SNAPSHOT_ARGS` registered under root
  alongside `node`. Marked `mcp_enabled = true`, `agent_safe = true`,
  `idempotent = true`.
- New dispatcher `dispatch_fleet_snapshot` fans out to the local
  daemon's existing endpoints:
    - `/api/v1/nodes`
    - `/api/v1/projects/{p}/agents` per project
    - `/api/v1/projects/{p}/beads?status=open,in_progress` per project
    - `/api/v1/feed?limit=N` (bounded; default 200, hard-cap 2000,
      `--feed-tail 0` disables)
- Snapshot envelope: `{ snapshot_at, node, cli_version, projects,
  nodes, agents, beads, feed_tail, errors }`. Errors are non-fatal:
  any individual endpoint failure is recorded under `errors[]` so a
  partial snapshot is still useful for postmortem.
- Output modes:
    - default: pretty JSON to stdout (paginated).
    - `--output PATH`: writes to file, prints summary.
    - `--json`: wraps the snapshot in `{ ok, data }`.
    - `--projects p1,p2`: restricts the scope.
- Inline fix: 3 `AgentSnapshot` test fixtures in
  `crates/caco-daemon/src/ui_stream.rs` (lines 3266, 3308, 4923) got
  `tmux_history_limit + tmux_history_size: None` so caco-daemon lib
  tests compile after bd-7ef076's daemon-side struct change.
- Two new tests:
    - `fleet_snapshot_help_json_advertises_optional_flags` — locks the
      `--output / --feed-tail / --projects` surface in the help-json
      contract so MCP / agents can rely on it.
    - `fleet_snapshot_rejects_feed_tail_over_cap` — exit 1 +
      `invalid_argument` when `--feed-tail > 2000`. Fails fast before
      the daemon round-trip.

## Validation

- `cargo test -p caco-cli --lib fleet_snapshot`: 2/2 PASS.
- `cargo test -p caco-daemon --lib msg_`: 17/17 PASS (incl. my prior
  `msg_broadcast_returns_within_one_second`).
- `cargo clippy -p caco-cli -p caco-daemon -p caco-web --all-targets
  -- -D warnings`: clean.
- `cargo test-small`: BLOCKED by bd-7ef076 caco-tui breakage (85
  errors). Pre-existing on origin/main. Filed bd-d46eb7. Verified by
  stashing my work, running test-small on origin/main → same 87 cli
  failures + caco-tui compile errors.
- `cargo test -p caco-cli --lib` full suite: 87 pre-existing failures
  on origin/main (`unable to resolve local node`, env state pollution).
  My two new tests pass when filtered alone.

## Diff summary

```
crates/caco-cli/src/lib.rs           | ~+220 (1 spec block, 1 root
                                     |        register, 1 dispatcher,
                                     |        2 tests)
crates/caco-daemon/src/ui_stream.rs  | +6  (3 fixtures × 2 fields)
.cacophony/agent/.../summary/0013    | (new)
```

## Operator-takeaway

`caco fleet snapshot --json | jq` now dumps a single postmortem
envelope. `caco fleet snapshot --output snap-$(date +%s).json` writes
it to disk for sharing. The non-fatal `errors[]` array means a partial
snapshot under partial-daemon-failure is still produced — exactly the
case the bead was filed to address.

Caveats:
- Per-node disk/load + DB row counts are not in this slice (no
  endpoint). The `nodes` field carries whatever `/api/v1/nodes`
  returns (versions, liveness, last_seen).
- `--replay` mode is deferred.
- caco-tui broken-on-main from bd-7ef076 is a real footgun for every
  agent doing test-small validation. bd-d46eb7 (P2) filed.

## Coordination

- Spoke heads-up about bd-7ef076 cascade and bd-d46eb7 follow-up.
- Will speak completion + reintegrate.

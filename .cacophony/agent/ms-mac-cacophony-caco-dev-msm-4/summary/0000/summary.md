# Session summary — bd-0502bc

## Goal
Land bd-0502bc (P3, caco-cli): fix the misnamed `caco fleet disk` subcommand. The surface only ever read local-node telemetry despite the cluster-wide "fleet" prefix, misleading operators. Solution: (1) promote `caco node disk` as the canonical surface, (2) keep `caco fleet disk` working as a deprecated alias with a one-line hint, (3) add `--node` support (previously warned-and-ignored per bd-b76723) with validation for local-node-only.

## Bead(s)
- **bd-0502bc** (P3, caco-cli): rename `caco fleet disk` to canonical `caco node disk`. Primary bead.

## Before state
- `caco fleet disk` help text admitted it was "for the local node" yet lived under the cluster-wide "fleet" prefix.
- `--node` was bd-b76723-warned-and-ignored: any `--node <name>` produced a warning but was silently ignored, always reading local-node telemetry.
- No `node disk` subcommand existed.
- No deprecation surface for `fleet disk`.

## After state
- NEW: `caco node disk` registered in `NODE_SUBCOMMANDS` with full arg spec (`--top`, `--node`). No deprecation banner.
- DEPRECATED: `caco fleet disk` retained in `FLEET_SUBCOMMANDS`. Summary text advertises DEPRECATED status and points to `caco node disk`. Text mode emits a one-line note before the table.
- `--node` validation:
  - Empty value rejected: "--node must not be empty (bd-0502bc)".
  - Non-local node name rejected with clear error naming the local node and deferring cluster-aggregation to a follow-on bead.
- JSON mode adds: `node: <local_node_name>`, `deprecated_surface: <bool>`, `canonical_surface: "caco node disk"`.
- `dispatch_fleet_disk` signature extended to `(top, node_filter, legacy_alias, json_requested, config_override)`, routing both surfaces through shared implementation.

## Tests
- New test `bd0502bc_node_disk_subcommand_is_registered`: pins node disk registration + MCP/agent-safe/idempotent flags + --top/--node presence.
- New test `bd0502bc_fleet_disk_alias_accepts_node_flag`: pins --node presence on legacy alias + DEPRECATED/canonical_surface strings in summary.
- New test `bd0502bc_node_disk_rejects_empty_node_value`: pins empty --node rejection in both text and JSON modes.
- New test `bd0502bc_node_disk_rejects_non_local_node`: pins end-to-end rejection of a non-local --node value via real dispatch path.
- Existing `fleet_disk_subcommand_is_registered` still passes (alias unchanged).
- Build: `cargo build -p caco-cli --tests` clean (post bd-ee2dd4 fix).

## Diff summary
- `crates/caco-cli/src/lib.rs`: +324/-20 lines across:
  - New `NODE_DISK_ARGS` const (bd-0502bc marker).
  - Updated `NODE_SUBCOMMANDS` with new "disk" entry.
  - Updated `FLEET_SUBCOMMANDS` disk entry with DEPRECATED summary.
  - Extended `FLEET_DISK_ARGS` with --node (deprecation pointer version).
  - Extended `dispatch_fleet_disk` signature and body for node_filter, legacy_alias handling, deprecation note, JSON envelope changes, --node validation.
  - Dispatcher routing for both `fleet disk` and `node disk` with shared logic.
  - 4 new bd0502bc_* tests.

## Operator-takeaway
Every operator who typed `caco fleet disk --node helsinki` and got warned-then-ignored now gets a clear "not the local node" error with guidance. Every operator discovering the surface via `caco fleet` help now sees "DEPRECATED — use `caco node disk` instead". The cluster-aggregation surface (actual per-peer telemetry via daemon RPC) remains deferred to a follow-on bead, but the naming and operator affordances are now honest.

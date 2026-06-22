# Session summary — agent_groups on /api/v1/node (bd-f6dc8a)

## Goal

The macOS companion's group-chat UI (bd-a66aff) needs the configured agent
groups, but the /api/v1/node payload it consumes did not carry them.

## Bead

- `bd-f6dc8a` (P2 task) — expose agent_groups on /api/v1/node.

## Finding (corrected the bead's premise)

The bead said "the /api/v1/node ProjectInfo struct does NOT carry agent_groups."
But /api/v1/node returns `NodeInfo` (verified: macOS DaemonClient.swift:253
decodes `DaemonEnvelope<NodeInfo>` from /api/v1/node), which is NODE-LEVEL and
has NO per-project payload at all — there is no "ProjectInfo" there. (The macOS
app DOES also have a snapshot() using /api/v1/ui/snapshot, whose per-project
ProjectSnapshot already carries agent_groups; but the group-chat UI builds from
/api/v1/node.) So the fix is to add a node-level agent_groups map to NodeInfo.

## After state

- Added `agent_groups: BTreeMap<String, BTreeMap<String, Vec<String>>>` (project
  -> { group -> [member ids] }) to `NodeInfo`, populated in `handle_node` via the
  same `config.resolve_project_agent_groups(project, cluster_groups)` resolution
  /api/v1/ui/snapshot uses (cluster groups + per-project overrides). Config-only
  resolution, cheap. Low cascade: NodeInfo is constructed once (one site).
  `#[serde(default)]` + always serialized (empty `{}` when no groups).

## Diff summary

- Code commit: `bd-f6dc8a: expose configured agent_groups on /api/v1/node`.
  Final landed squash SHA from the reintegration receipt.
- File: `crates/caco-daemon/src/lib.rs` (NodeInfo field + handle_node compute +
  node-endpoint test assertion).
- Test: `node_endpoint_returns_success_envelope` now asserts the agent_groups map
  is serialized; per-project resolution correctness is covered by caco-config's
  resolve_project_agent_groups tests.

## Operator-takeaway

/api/v1/node now carries the configured per-project agent groups, so the macOS
group-chat UI (and any /node consumer) gets the groups without switching to
/ui/snapshot. The macOS Swift side (caco-macos-1, bd-a66aff) decodes the new
`agent_groups` key. Shape: `{ project: { group: [memberIds] } }`.

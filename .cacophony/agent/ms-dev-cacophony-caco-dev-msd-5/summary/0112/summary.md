# Session summary — per-node expectation-aware health on /api/v1/nodes (bd-03fad6)

## Goal

`GET /api/v1/nodes` set each node's `liveness` to RAW reachability only; the
expectation-aware peer health (which honors `health_expectations.expected_unreachable`
to avoid alarming on offline-by-design nodes) was derived only for the fleet-summary
+ doctor, NOT per-node. So clients (macOS NodesPane) could not tell winmini
(config-marked `expected_unreachable`) from a genuine outage. Attach expectation-aware
health per-node so clients can render offline-by-design calmly — the daemon-side
enabler for bd-07de30 and Harry's resilient-to-offline-nodes directive.

## Bead(s)

- `bd-03fad6` — daemon: expose per-node expectation-aware health on /api/v1/nodes
  (unblocks bd-07de30 offline-by-design).

## Before state

- Failing tests: none.
- `handle_nodes` built each `NodeSummary` with `liveness: peer.status.as_str()`
  (raw). No `health_expectations` consultation per-node.

## After state

- Failing tests: none.
- `NodeSummary` gains `peer_health: NodeHealthSummary { status, expected, note }`,
  computed via `node_summary_health` (status = derived `peer_health_status`;
  `expected` = `health_expectations.expects_status(status)` — true e.g. for an
  `expected_unreachable` node that is unreachable; `note` = the operator note).
  Raw `liveness` is preserved. All 4 NodeSummary constructions (static peer/no-peer,
  dynamic peer/no-peer) wired. Dynamic nodes (no config) get `expected: false`.

## Diff summary

- File: `crates/caco-daemon/src/lib.rs` only.
- Added `NodeHealthSummary` struct, `node_summary_health` (config wrapper) +
  `node_health_summary` (pure), the `peer_health` field, and wired the 4
  constructions. +1 test (`node_health_summary_marks_expected_unreachable_calm`).
- Validation: cargo test -p caco-daemon --lib node_health_summary GREEN (1 passed);
  cargo check --workspace --tests on rebased content.
- Note: `handle_node_show` (/api/v1/nodes/{node}) returns a different shape (not
  NodeSummary); the macOS NodesPane consumes the /api/v1/nodes LIST, which is fixed
  here. handle_node_show + /ui/snapshot expectation-aware health are documented
  follow-ups if a consumer needs them.
- Final landed squash SHA from the reintegration receipt.

## Operator-takeaway

/api/v1/nodes now carries per-node expectation-aware health (`peer_health`), so
the macOS NodesPane (bd-07de30) and other clients can render an offline-by-design
node (winmini/aurora/pocket4/sonance, config-marked expected_unreachable) as
"offline (expected)" instead of an alarming outage, while raw `liveness` stays
available. Directly serves Harry's resilient-to-offline-nodes directive.

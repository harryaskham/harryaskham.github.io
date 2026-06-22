# Session summary — expectation-aware health on the UiSnapshot TopologyNodeSnapshot (bd-220a87)

## Goal

Follow-on to bd-03fad6: that added expectation-aware `peer_health` to NodeSummary on
the /api/v1/nodes DETAIL path, but the caco-web Nodes LIST (and the caco-tui Nodes view)
render from the daemon UiSnapshot's `TopologyNodeSnapshot` (ui_stream.rs), which carried
raw `status` but no expectation signal. Add per-node `peer_health_expected` + note to
TopologyNodeSnapshot so caco-web (bd-0e7c97) and caco-tui (bd-ac0366) can render an
offline-by-design node (config `expected_unreachable`) calmly instead of an outage.

## Bead(s)

- `bd-220a87` — [daemon] populate expectation-aware health on TopologyNodeSnapshot
  (web/UiSnapshot) for caco-web + caco-tui offline-by-design (bd-03fad6/bd-0e7c97/bd-ac0366).

## Before state

- Failing tests: none.
- `TopologyNodeSnapshot` had `status` (raw peer_health_status) only; the SSE-updated
  snapshot LIST/TUI had no `expected`/`note`.

## After state

- Failing tests: none.
- `TopologyNodeSnapshot` gains `peer_health_expected: bool` + `peer_health_note: Option<String>`
  (both `#[serde(default)]`), computed via `topology_node_health_expectation` (reusing
  `expects_status` + the note). Static caller computes from the node's `health_expectations`;
  dynamic peers (no config) get `(false, None)`. Raw `status` preserved.

## Diff summary

- File: `crates/caco-daemon/src/ui_stream.rs` only.
- Added the 2 fields, `topology_node_health_expectation` helper, threaded through
  `topology_node_from_parts` + its 2 callers, +1 test
  (`topology_node_health_expectation_marks_expected_unreachable_bd_220a87`).
- Validation: cargo test -p caco-daemon --lib topology_node_health_expectation GREEN;
  cargo check --workspace --tests on rebased content.
- Final landed squash SHA from the reintegration receipt.

## Operator-takeaway

The daemon UiSnapshot now carries per-node expectation-aware health on the topology
nodes, so the caco-web Nodes LIST (bd-0e7c97) and caco-tui Nodes view (bd-ac0366) can
render offline-by-design nodes calmly off one daemon field set. Converged caco-web +
caco-tui needs onto a single gated-caco-dev daemon change (gate-coverage). Completes
the offline-node-resilience daemon surface (with bd-03fad6 for /api/v1/nodes).

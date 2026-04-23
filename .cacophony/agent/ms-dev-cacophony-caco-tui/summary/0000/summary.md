# Session summary — direct claim attribution fix

## Goal

Make direct bead claims from operator-facing CLI and TUI sessions attribute to the human operator without being mistaken for stable managed-agent ownership, while preserving the existing stable assignee contract for real agent-owned claims.

## Bead(s)

- `bd-5d53b7` — Direct CLI/TUI claims should attribute to operator, not node+tui

## Before state

- Failing tests: none newly introduced; this session started from a green checkout for the targeted daemon paths.
- Relevant metrics: direct claim handling in `crates/caco-daemon/src/beads.rs` normalized every 3-part caller to stable agent format.
- Context: operator-originated callers such as `localhost:cacophony:alice` could be stored as `cacophony:alice`, which looks like a stable agent assignee and can trip stale-assignee / failover logic even though the claim came from a human CLI/TUI reservation.

## After state

- Failing tests: none in the targeted validation set.
- Relevant metrics: operator direct claims now compact to `node:actor`; managed-agent claims with provenance still normalize to stable `project:agent_id`; forwarded claim proxies preserve agent provenance headers.
- Context: the daemon now distinguishes direct operator reservations from managed-agent ownership at claim time, so CLI/TUI claims stay operator-attributed without colliding with agent-only reconciliation semantics.

## Diff summary

- Commits: `756f4ca1`
- Files touched: `crates/caco-daemon/src/beads.rs`, `crates/caco-daemon/src/lib.rs`
- Tests: +4 focused regression tests, 0 removed, 0 flipped
- Behavioural delta: `POST /api/v1/projects/<project>/beads/claim` now stores direct human claims as `node:actor` while preserving stable `project:agent_id` assignees for managed-agent claims. Forwarded claim requests also preserve `x-caco-agent-id` / `x-caco-agent-project` so authoritative beads hosts make the same decision as local daemons.

## Operator-takeaway

The fix is deliberately narrow: human CLI/TUI claims are now visibly owned by the operator without masquerading as live agents, and real managed-agent claims still keep the stable assignee shape that the rest of the bead lifecycle depends on.
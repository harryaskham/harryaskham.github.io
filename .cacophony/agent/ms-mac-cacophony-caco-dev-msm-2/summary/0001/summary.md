# Session summary — fix permanent auto-claim rollback route

## Goal

Repair the defensive rollback path that runs when `caco bd claim` with no bead ID receives a permanent tracker from a stale or remote daemon. The prior guard correctly refused the tracker, but attempted to unclaim it through an invalid endpoint.

## Bead(s)

- `bd-45cdba` — [bug] bd claim permanent-skip rollback uses wrong unclaim route

## Before state

- Failing tests: no regression existed for the rollback URL shape.
- Relevant metrics: live command after bd-0755d1 returned `Automatic unclaim failed: daemon returned invalid JSON (HTTP 405 Method Not Allowed)` while manual `caco bd unclaim --bead-id bd-72fd72` succeeded.
- Context: the rollback posted to `/api/v1/projects/<project>/beads/unclaim` with `bead_id` in the body, but the daemon exposes `POST /api/v1/projects/<project>/beads/<bead_id>/unclaim`.

## After state

- Failing tests: none in targeted or small validation.
- Relevant metrics: `timeout 120 cargo test -p caco-cli --lib auto_claimed_permanent_rollback_url_uses_bead_unclaim_route_bd_45cdba -- --nocapture` passed; `timeout 180 cargo test-small` passed.
- Context: rollback now builds the same bead-specific URL shape as `caco bd unclaim`, includes an audit reason, and wraps transport errors with the original no-ready explanation.

## Diff summary

- Commits: a64d4fc28
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added `auto_claimed_permanent_rollback_url_uses_bead_unclaim_route_bd_45cdba`.
- Behavioural delta: if a stale daemon still auto-assigns a permanent tracker, the client-side guard can roll it back through the valid unclaim route instead of leaving an accidental assignee behind.

## Operator-takeaway

The permanent-tracker guard now fails safely end-to-end: it refuses the bad auto-claim and uses the real unclaim endpoint to clear ownership, preventing workers from getting stuck holding tracker beads during queue drain.

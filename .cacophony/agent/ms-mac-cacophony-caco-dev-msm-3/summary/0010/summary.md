# Session summary — bd-8bb05a notifications-table retention pruning

## Goal

`crates/caco-daemon/src/notifications.rs` creates the `notifications` table with a `ts` column but `store.rs::prune_retention` had no DELETE clause for it. Every operator notification (info/warning/error) ever inserted lived forever, growing the daemon DB unbounded.

## Bead(s)

- `bd-8bb05a` — [bd-20381a child] notifications table has no retention pruning — operator notifications accumulate forever.

## Before state

- `notifications` table had no retention. Acked + unacked rows accumulated indefinitely.
- `PruneStats` had no `notifications` field. Display-impl showed no notifications counter.

## After state

- Two new constants in `store.rs`:
  - `RETENTION_NOTIFICATIONS_ACKED_DAYS = 30`
  - `RETENTION_NOTIFICATIONS_UNACKED_DAYS = 90`
- `prune_retention` runs two DELETE passes:
  1. `DELETE FROM notifications WHERE acknowledged = 1 AND acknowledged_at < ?` (30d cutoff). Operator already triaged — no reason to keep indefinitely.
  2. `DELETE FROM notifications WHERE acknowledged = 0 AND ts < ?` (90d cutoff). Longer than acked because unacked is the operator's queue and shouldn't silently disappear after a holiday; 90d is conservative.
- New `PruneStats.notifications: usize` field (sum of both passes); included in `total()` and the `Display` impl.
- Doc-comment for `prune_retention` updated.

## Diff summary

- Files touched: `crates/caco-daemon/src/store.rs` only.
- Commit: `<TBD>`.
- Tests:
  - New `prune_retention_deletes_old_notifications` — boundary-precise: inserts 5 rows (acked-31d, acked-29d, unacked-91d, unacked-89d, acked-fresh), asserts `stats.notifications == 2`, asserts the surviving 3 rows are exactly the expected IDs.
  - Existing `prune_stats_display` updated for the new field (initializer-completeness check).
  - All 7 `prune_retention*` tests pass.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean.

## Operator-takeaway

Acked operator notifications older than 30 days will now disappear on the next prune cycle (which runs at startup + via `caco db prune`). Unacked notifications older than 90 days will also disappear — if you have a long-tail backlog of unacked notifications you cared about, ack or escalate them before this lands.

If 90 days proves too aggressive for the unacked window, the constant is a one-line tweak.

## Sister beads

bd-713cd8 (images table retention) and bd-a0a229 (note_delivery_tracking retention) are the other open bd-20381a children — same change pattern. Whoever picks them up next can mostly copy this PR.

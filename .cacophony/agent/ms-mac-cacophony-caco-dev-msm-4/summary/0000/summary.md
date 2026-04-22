# bd-ff59fa — caco summary undercount + caco bd list --updated-since default sort

## Goal
Make `caco summary` accurately reflect bead-close activity, and make
`caco bd list --updated-since N` show the newest activity first by
default.

## Bead(s)
- bd-ff59fa (P2 bug, label test-user) — `caco summary undercounts
  closed beads — reports '0 closed' in 1h window when ~10 beads
  actually closed`. Bonus finding in same bead: default sort for
  `--updated-since` returns oldest beads first.

## Before state
- Reproduced on this host:
  `caco summary --since 1h` → `Beads: 7 created, 9 claimed, 0 closed`
  while `sqlite3 daemon.db "SELECT COUNT(*) FROM feed_events WHERE
  ts >= '2026-04-22T13:00:00' AND event_type='bead_closed'"` returned
  many real closures from earlier in the window.
- Root cause: `caco bd update --status closed` (handler
  `handle_update_bead`) emitted `EventType::BeadUpdated` regardless
  of the new status, while `caco bd close` (handler
  `handle_close_bead`) emitted `EventType::BeadClosed`. The
  /api/v1/summary aggregator counts `bead_closed` rows only, so
  every closure done via the update path was invisible to it.
  Operators routinely close via `update --status closed` for
  housekeeping (it's the recommended path when the dedicated close
  validator can't find the bead in main, e.g. bd-845653 workaround),
  so the undercount was systematic.
- Bonus bug: `caco bd list --updated-since 1h` returned ancient
  unchanged beads at the top of the window because the static
  default sort was `Priority` and `reverse=false`. An operator
  looking for "what moved recently?" would scan the top of the
  list and conclude `--updated-since` was broken.

## After state
- `handle_update_bead` now emits `EventType::BeadClosed` (instead of
  `BeadUpdated`) when both:
  1. The request's `status` was `BeadStatus::Closed`, and
  2. The post-update bead's `.status` is in fact `Closed`.
  All other update shapes still emit `BeadUpdated` unchanged. The
  cross-project move arm is unaffected (already separate).
- New helper `resolve_bead_sort(sort, reverse, updated_since_active)
  -> (BeadSortField, bool)`. When `--updated-since` is in play and
  neither `--sort` nor `--reverse` was explicitly passed, defaults
  to `(UpdatedAt, reverse=true)`. Explicit `--sort` or `--reverse`
  always wins so power users keep full control.
- Helper plumbed into both `handle_list_beads` (per-project) and the
  global beads list path. Legacy callers without `--updated-since`
  still get the original `(Priority, reverse=false)` default.

## Diff summary
- `crates/caco-daemon/src/beads.rs` (+98/-5):
  - `handle_update_bead` non-move arm: pick `BeadClosed` vs
    `BeadUpdated` based on requested+resulting status.
  - New `resolve_bead_sort` helper between `parse_bead_sort` and
    `mainline_validation_message`.
  - Two `let (sort_by, reverse) = resolve_bead_sort(...)` call sites
    (per-project list at L2598-region, global list at L3797-region).
  - 4 unit tests in `beads::tests`:
    - `resolve_bead_sort_updated_since_defaults_to_updated_at_reverse`
    - `resolve_bead_sort_explicit_sort_wins`
    - `resolve_bead_sort_explicit_reverse_wins`
    - `resolve_bead_sort_no_updated_since_keeps_legacy_default`

## Operator-takeaway
- After this rolls out, `caco summary --since N` will accurately
  count closures done via either `caco bd close` or
  `caco bd update --status closed`. Existing automation that
  watched the `bead_closed` feed event already saw both lifecycle
  paths only when the operator used the dedicated close command —
  it'll now also see them when housekeeping closes go through
  update. Workers that derive state from `bead_updated` rows
  specifically (none known in tree) would need to add a
  `bead_closed` listener too.
- `caco bd list --updated-since 1h` now top-loads recent activity
  by default. To restore the old priority-default ordering pass
  `--sort priority` explicitly. To scroll up through history pass
  `--reverse=false` (the existing flag's negative form).

## Tests
- `cargo test -p caco-daemon --lib resolve_bead_sort` — 4/4 passed.
- `cargo build -p caco-daemon` — clean.
- `cargo clippy -p caco-daemon --all-targets -- -D warnings` — clean.
- End-to-end behaviour change requires a daemon binary roll on the
  node owning the cacophony beads DB before `caco summary` will
  reflect the fix; existing closed-via-update events from before the
  roll remain invisible (they're already-stored `bead_updated`
  rows). Going forward, every fresh closure is counted.

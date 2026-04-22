# bd-a3ee9d — TUI sidebar Closed/Draft counts now correct

## Goal
Stop the TUI/web sidebar from rendering Closed (0) / Draft (0)
when the underlying store has thousands of closed and draft beads.

## Bead(s)
- bd-a3ee9d (P2 bug), Option A from the design notes:
  ship per-status counts in the snapshot and have the TUI read
  badge totals from those counts, not from the trimmed bead list.
  Option B (lazy fetch on subsection click) deferred.

## Before state
- Daemon: `compute_bead_stats` only tallied open / in_progress /
  closed / blocked. `ProjectBeadStats` had no draft or permanent
  fields. `trim_snapshot_beads` (bd-ecf1a0) drops closed/draft
  beads from the wire payload.
- TUI: `bead_counts_global` / `bead_counts_for_project` iterated
  `self.beads` (the trimmed set) so Closed / Draft buckets always
  read 0. During the bd-cf99b7 incident this fooled the operator
  into spending ~30 min investigating phantom data loss while
  `/api/v1/beads` correctly reported 1054 closed beads on disk.

## After state
- Daemon: `ProjectBeadStats` grows `draft` and `permanent` fields
  (`#[serde(default)]` for backwards compatibility with older
  TUI clients). `compute_bead_stats` tallies draft + permanent.
  Stats are still computed from the unfiltered store BEFORE the
  snapshot trim runs.
- TUI: `bead_counts_global` / `bead_counts_for_project` retain
  iteration over `self.beads` for buckets that survive the trim,
  then override `closed` and `draft` from `self.bead_stats`
  (sum across projects for global; matching `ProjectBeadStats`
  entry per-project). `total` is recomputed from the corrected
  per-section sum so the badge total is consistent with the
  per-section badges. The override is conditional
  (`stats > 0 || bucket == 0`) so a TUI talking to a
  not-yet-trimming daemon (mixed-version cluster) doesn't
  regress.

## Diff summary
- `crates/caco-daemon/src/ui_stream.rs` (+~30):
  - `ProjectBeadStats { draft, permanent }`.
  - `compute_bead_stats` arms for `"draft"` and `"permanent"`.
  - New unit test `compute_bead_stats_counts_draft_and_permanent`.
- `crates/caco-tui/src/state/mod.rs` (+~30):
  - `bead_counts_global` + `bead_counts_for_project` override
    `closed` / `draft` from `bead_stats` and recompute `total`.
- `crates/caco-tui/src/state/tests.rs` (+~85):
  - 2 new tests asserting the override surfaces correct totals
    when `self.beads` is trimmed.
- `crates/caco-tui/src/app/benchmark_support.rs`,
  `crates/caco-tui/src/views/project_overview.rs`: synthetic
  fixtures grow the two new fields.

## Tests
- `cargo build -p caco-daemon -p caco-tui` — clean.
- `cargo clippy -p caco-daemon -p caco-tui --all-targets -- -D warnings` — clean.
- 3/3 new tests pass.

## Operator-takeaway
After the binary rolls, the TUI/web sidebar's Closed and Draft
badge counts will match what `caco bd list --status closed` and
`caco bd list --status draft` report. The wire snapshot stays
trimmed (still small per bd-ecf1a0); only the badge counts gain
the missing buckets via `bead_stats`. Lazy-load of closed bead
bodies on subsection click is the remaining follow-up Option B
on the bead — left open.

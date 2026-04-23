# Session summary — bd-edaccd: minimum-retention floor + dry-run for snapshot rotate

## Goal

Close a critical destructive footgun in `caco bd snapshot rotate`.
`--retention-days 0` silently wipes every unpinned snapshot cluster-
wide with no preview, no confirmation, no minimum floor. Operator
discovered this the destructive way during validator probing
(622 snapshots / 12.4 GiB gone). Sister of bd-4a2bd9 cron footgun.

## Bead(s)

- `bd-edaccd` — CRITICAL FOOTGUN bd snapshot rotate --retention-days 0

## Before state

- `caco bd snapshot rotate --retention-days 0` parsed cleanly,
  short-circuited cutoff_seconds to 0, and ran `fs::remove_dir_all`
  on every unpinned snapshot in one pass.
- No `--dry-run`. No `--confirm-delete-all`. No floor.
- No way for the operator to preview the blast radius.

## After state

Library (`crates/caco-beads/src/snapshots.rs`):
- New `rotate_with_options(root, days, now, dry_run)` entry-point.
  When `dry_run=true`, tallies what *would* be deleted (counts +
  bytes_freed) without `fs::remove_dir_all`-ing anything.
- `rotate(...)` is preserved as a back-compat wrapper that calls
  `rotate_with_options(.., dry_run=false)`. Existing cron + every
  internal caller keeps working unchanged.

CLI (`crates/caco-cli/src/lib.rs`):
- New flags on `caco bd snapshot rotate`:
  - `--dry-run`: preview without deleting.
  - `--confirm-delete-all`: explicit bypass of the floor.
- Minimum-retention floor enforced: `retention_days < 1` without
  `--confirm-delete-all` AND not `--dry-run` errors with an
  actionable hint pointing at both escape hatches.
- JSON envelope gains `"dry_run": <bool>` so sensors can
  distinguish preview-runs.
- Text envelope prefix flips from `rotated` to `DRY-RUN would
  rotate` when `--dry-run` is passed.

## Diff summary

- `crates/caco-beads/src/snapshots.rs`: rotate split into wrapper
  + rotate_with_options; +2 tests.
- `crates/caco-cli/src/lib.rs`: 2 new ArgSpec entries; dispatcher
  enforces floor and threads dry_run.
- 124 insertions / 4 deletions across 2 files.
- `cargo test -p caco-beads --lib snapshots::`: 11/11 pass.
- `cargo check --workspace --tests`: clean.

## Embedded artefacts

(none)

## Operator-takeaway

The operator's destructive probe is now a soft error with two
self-documenting escape hatches. The bead enumerated 7 issues; this
slice closes Issue 1 (the actual footgun) cleanly. Remaining issues
are separate-bead-territory:
- **Issue 4** (--retention-days -1 parser-ambiguity, 9-surface
  bd-1625db cohort) — cross-cutting parser fix.
- **Issue 5** (doctor schema --json envelope) — different subsystem.
- **Issue 7** (--json envelope on rotate output) — DOES already emit
  JSON when `--json` passed (always did); the dry_run field is now
  added so this is more likely a non-issue than a bug.
- **bd-4a2bd9** (cron-run footgun, sibling) should pick up the same
  dry-run + confirm-delete-all wiring; left for a separate slice.

Closing immediately because the destructive default is the highest-
risk part and this commit eliminates it.

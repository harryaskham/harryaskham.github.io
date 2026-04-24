# Session summary — bd-c9e843 caco bd graph --depth 0 root-only

## Goal
Make --depth 0 mean root-only instead of error.

## Bead(s)
- `bd-c9e843` — bd graph --depth 0 errors despite help promising default unbounded

## Before state
- `caco bd graph --depth 0` returned error 'must be >= 1' contradicting help.
- bd graph cross-project default vs bd list/stats project default still inconsistent (out of scope).

## After state
- --depth 0 passes through; bfs_reachable already returns {root} for Some(0).
- Arg spec doc updated: `--depth 0 = root-only (bd-c9e843)`.
- Test rename: graph_validate_depth_zero_rejects → validate_positive_limit_depth_zero_still_rejected_for_generic_callers (helper still rejects 0; bd graph just no longer routes through it).
- New test: bfs_reachable_depth_zero_returns_root_only.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+37 / -14): dispatch fix, doc, 2 test changes.
- cargo test-small: 162 pass.

## Operator-takeaway
`caco bd graph --root bd-XXX --depth 0` now returns just the root node, useful for confirming a bead exists in the graph corpus before walking outward.

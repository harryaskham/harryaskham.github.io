# Session summary 0000 — bd-274c2d cycle 0027: dup on_revival + too_many_arguments + doc_overindented_list_items

## Goal

Workspace clippy red on origin/main with three errors. Sweep.

## Bead(s)

- `bd-274c2d` — Permanent: continuous test suite health (broken-on-main sweep cycle 0027).

## Before state

- `cargo clippy --workspace --all-targets -- -D warnings`: failing.
  1. `E0062: field on_revival specified more than once` at `caco-cli/src/lib.rs:80244` — msd-1 added `on_revival: None` to test fixture but msm-3's bd-274c2d cycle 0024 had already added it to the same fixture; got duplicated.
  2. `clippy::too_many_arguments (8/7)` on `dispatch_agent_logs` at `lib.rs:31502` (mab6 bd-d4e93d caco msg history added 8th arg or similar dispatch; same lint pattern as bd-83a8ed).
  3. `clippy::doc_overindented_list_items` at `lib.rs:42061` — bullet item continuation overindented from 5 → 15 spaces in `caco msg history` doc-comment.

## After state

- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `cargo test-small`: 56 pass.

## Implementation

1. Removed line 80244 (the duplicate `on_revival: None,`).
2. Added `#[allow(clippy::too_many_arguments)]` above `fn dispatch_agent_logs`.
3. Reformatted the `--grep` bullet's continuation line to use 5-space indent (matching the bullet body offset).

## Diff summary

- `crates/caco-cli/src/lib.rs` — 3 small edits.

## Operator-takeaway

Triple-symptom sweep, all from concurrent test-fixture / doc-comment landings. The dup-field is a cycle-0024 vs msd-1 race on the same fixture; the `too_many_arguments` and `doc_overindented_list_items` are msd-1 bd-d4e93d (caco msg history) collateral.

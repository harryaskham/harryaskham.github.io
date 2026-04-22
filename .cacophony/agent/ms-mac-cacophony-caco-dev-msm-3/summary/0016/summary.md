# Session summary — bd-274c2d cycle: post-msd-4 broken-on-main wave

## Goal

After the previous bd-274c2d sweep landed, msd-4's interleaved reintegrate added a fresh wave of broken-on-main errors. Took the cycle immediately to keep the workspace green for everyone.

## Bead(s)

- `bd-274c2d` — Permanent: continuous test suite health (one cycle).

## Before state

- `cargo clippy --workspace --all-targets -- -D warnings`: 5 errors:
  1-2. `E0062 field tmux_history_limit/size specified more than once` in `crates/caco-daemon/src/ui_stream.rs:3417` — msd-4 added the same fields to a fixture I'd already added them to in the previous cycle.
  3. `E0063 missing fields tmux_history_limit/size` in another `AgentSnapshot` literal at `ui_stream.rs:3428` — msd-4 added a new fixture without the fields.
  4. `E0063 missing field disable_hooks` in `crates/caco-cli/src/lib.rs:76607` — `caco_profile::Profile` got a new `disable_hooks: Option<Vec<String>>` field that didn't sweep the `test-fast-gate` fixture.

## After state

- Removed the duplicate `tmux_history_limit/size: None` lines (kept the first occurrence) in the `agent_snapshot_with_checkout_path_round_trips` fixture.
- Added the missing two fields to `agent_snapshot_without_checkout_path_round_trips`.
- Added `disable_hooks: None` to the test-fast-gate `Profile` literal in caco-cli (alongside `short_name_strategy: None` from the earlier cycle).
- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `cargo test-small`: 56 pass.

## Diff summary

- `crates/caco-daemon/src/ui_stream.rs` — duplicate-line cleanup + 2-line fixture addition.
- `crates/caco-cli/src/lib.rs` — 1-line fixture addition.
- Commit: `<TBD>`.

## Operator-takeaway

Fourth broken-on-main sweep this session, third struct-field-add miss. Same pattern as bd-274c2d's previous cycles. The merge-queue daemon (bd-2c399b) gating clippy pre-merge would catch all of these. The duplicate-field case here is a particular bear-trap: when two agents independently sweep the same field-add the second to land hits E0062 instead of E0063, which doesn't surface until rebase. Worth folding into bd-2c399b's gate semantics: post-rebase clippy must pass before squash-merge.

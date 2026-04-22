# Session summary — drive-by fix for bd-ae6b7b duplicate fields

## Goal

Unblock `cargo check --workspace --tests` after ms-dev-companion's bd-ae6b7b sweep left 3 AgentSnapshot literal sites with `tmux_history_limit` / `tmux_history_size` inserted twice.

## Bead(s)

- `bd-ae6b7b` — [main is broken] AgentDisplayState/AgentSnapshot tmux_history_limit/size missing (parent, claimed by ms-dev-companion; this is a drive-by tail).

## Before state

- After companion's bd-ae6b7b sweep landed (ea53f7f6), `cargo check --workspace --tests` failed with 6 E0062 errors: `field tmux_history_limit specified more than once` and `field tmux_history_size specified more than once` at `crates/caco-daemon/src/ui_stream.rs:3417 / 3463 / 5082`.
- Cause: the sweep inserted the new fields after both `provider: None,` AND `model: None,` in the same struct literal at 3 sites.

## After state

- Single occurrence per literal, placed after `model: None,` to match the canonical AgentSnapshot field ordering used elsewhere in the file.
- `cargo check --workspace --tests`: clean.
- `cargo test-small`: 209 / 109 / 739 / 291 / 18 / 2817 / 56 — all green.

## Diff summary

- Commit: `46822c17`
- Files touched: `crates/caco-daemon/src/ui_stream.rs` (-6 lines).
- Tests: 0 added; 0 removed; 0 flipped.
- Behavioural delta: none — test-only fixture cleanup.

## Operator-takeaway

Workspace test-check is green again. Companion still owns bd-ae6b7b parent (AgentDisplayState sweep across caco-tui).

# Session summary 0000 — bd-274c2d cycle 0028: dup operator-actions list dispatch

## Goal

Workspace clippy red on origin/main with E0428 (duplicate `dispatch_operator_actions_list` definition) and E0061 (call mismatch). msm-5 bd-6b7b30 slice 1 added a parallel implementation of `caco operator-actions list` that was already implemented under bd-8fe920. Sweep.

## Bead(s)

- `bd-274c2d` — Permanent: continuous test suite health (broken-on-main sweep cycle 0028).

## Before state

- `cargo clippy --workspace --all-targets -- -D warnings`: failing.
  - `E0428: dispatch_operator_actions_list defined multiple times` (16747 vs 51241).
  - `E0061: this function takes 3 arguments but 2 were supplied` (call site at 10057).

## After state

- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `cargo test-small`: 56 pass.

## Implementation

bd-8fe920 already shipped `dispatch_operator_actions_list(project: &str, json_requested: bool, config_override: Option<&PathBuf>)` with a proper daemon-API call at 16747, and a dispatch site at 10106 using `co`. msm-5 bd-6b7b30 slice 1 landed a parallel implementation at 51241 that shells out (`std::process::Command`) and a parallel dispatch arm at 10050 that uses a 2-arg signature. Both the duplicate dispatch arm and the duplicate function are dead — the bd-8fe920 path is canonical and superior (no shell-out, uses daemon API directly).

Changes:
- Deleted the second `fn dispatch_operator_actions_list` (51241 onwards) and its doc-comment.
- Deleted the duplicate match arm at 10050 (kept the bd-8fe920 arm at 10106).

## Diff summary

- `crates/caco-cli/src/lib.rs` — ~110 lines removed (one fn + one match arm + comments).

## Operator-takeaway

Concurrent landing collision. msm-5 bd-6b7b30 implemented `operator-actions list` independently of bd-8fe920 (already shipped). Kept the canonical bd-8fe920 implementation; dropped the bd-6b7b30 shell-out duplicate. Behaviourally equivalent for the operator (same CLI surface, same JSON envelope), with the bd-8fe920 path slightly faster (one HTTPS round-trip vs sub-process spawn).

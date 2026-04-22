# Session summary — bd-274c2d cycle 0024: Profile.on_revival missing field

## Goal

`cargo clippy --workspace --all-targets -- -D warnings` failing on origin/main with `E0063: missing field on_revival in initializer of caco_profile::Profile` at `caco-cli/src/lib.rs:79443` (test-fast-gate fixture). Sweep and unblock.

## Bead(s)

- `bd-274c2d` — Permanent: continuous test suite health (broken-on-main sweep cycle 0024).

## Before state

- `cargo clippy --workspace --all-targets -- -D warnings`: failing.
- `crates/caco-profile/src/model.rs:501` adds `pub on_revival: Option<String>` to `Profile` (msm-5 bd-1ac1b7 follow-up to bd-d5d63b — daemon last-N-events checkpoint + profile on_revival hook).
- One test-only fixture in `caco-cli/src/lib.rs` constructed `Profile { ... }` literally without the new field.

## After state

- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `cargo test-small`: 56 pass.

## Implementation

Inserted `on_revival: None,` before the closing brace of the test-fast-gate `Profile` literal in `caco-cli/src/lib.rs` via Python brace-walker.

## Diff summary

- `crates/caco-cli/src/lib.rs` — 1 line.
- Commit: `<TBD>`.

## Operator-takeaway

Single-fixture sweep (msm-5 added the field but only in `model.rs`; this picks up the one downstream constructor). Cycle 0024.

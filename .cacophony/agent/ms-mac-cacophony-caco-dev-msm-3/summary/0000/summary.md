# Session summary — Canonical node show missing-name error

## Goal

This session aligned `caco node show` with the post-bd-45d1f7 canonical missing-required-argument error style used by sister CLI surfaces.

## Bead(s)

- `bd-c3fcdd` — caco node show no-arg error wording drift

## Before state

- Failing tests: no test existed for `caco node show` without a target.
- Relevant metrics: the command emitted `usage: caco node show <node> (or --node/--name)` instead of the required-name plus discoverability pointer wording.
- Context: `--name ''` and unknown-node paths already had better discoverability; only the missing-target path used the old usage hint.

## After state

- Failing tests: none in scoped validation.
- Relevant metrics: targeted regression `node_show_missing_target_uses_required_name_error` passed; `cargo check -p caco-cli --lib` passed; `cargo test-small` passed with 256 tests before replay.
- Context: missing target now says `--name is required for node show ... Run caco node list ...`.

## Diff summary

- Commits: `5aa666130`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added exact error-message regression coverage.
- Behavioural delta: no-arg `caco node show` now matches the canonical missing-argument/discoverability template.

## Operator-takeaway

A small CLI consistency miss from the required-argument sweep is fixed and pinned with an exact regression.

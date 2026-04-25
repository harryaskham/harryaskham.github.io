# Session summary — reintegration policy deduplication

## Goal

Reduce drift between `caco agent complete` and `caco agent reintegrate` by extracting their duplicated reintegration policy resolution into one helper. This keeps future changes to mode defaults, allowed modes, PR-backend routing, tag pushing, and auto-rebase retry limits from being applied to one lifecycle path but missed in the other.

## Bead(s)

- `bd-626b48` — Deduplicate agent complete/reintegrate policy resolution

## Before state

- Failing tests: none known for this refactor.
- Relevant metrics: both dispatch paths carried separate copies of profile/env/project mode resolution, allowed-mode enforcement, direct PR-backend selection, `push_tags`, and `auto_rebase_retry_limit` lookup.
- Context: the duplicate code lived in `crates/caco-cli/src/lib.rs` inside `dispatch_agent_complete` and `dispatch_agent_reintegrate`.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: one `ResolvedAgentReintegrationPolicy` helper now resolves mode, recorded flag, direct PR-backend routing, tag pushing, and auto-rebase retry limit for both lifecycle paths.
- Context: the two dispatch paths now only unpack the resolved policy before constructing their existing reintegration requests.

## Diff summary

- Commits: `45b3f9d92`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: `cargo fmt --all -- --check`; `cargo check -p caco-cli --tests`; `cargo test -p caco-cli allowed_reintegration_modes_block_disallowed_selection --lib`
- Behavioural delta: no intended user-facing behavior change; this is an internal refactor to make complete/reintegrate policy behavior stay consistent.

## Operator-takeaway

The lifecycle policy decision point is now shared between complete and mid-flight reintegrate, reducing the chance that future PR-backed or direct-merge policy fixes only land on one path.

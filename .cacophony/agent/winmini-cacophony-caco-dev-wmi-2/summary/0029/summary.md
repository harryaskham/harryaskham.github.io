# Session summary — bd-e52dd2 release logs alias/help parity

## Goal

Bring `caco release logs` back into parity with its sibling `caco release status` for the small but sharp operator affordances around release job IDs: the same alias surface, the same discoverability-oriented missing-argument wording, and no warn-then-ignore behaviour when an operator naturally tries `--job-id` or `--release-id`.

## Bead(s)

- `bd-e52dd2` — caco release logs asymmetric vs sibling release status: aliases/help/error parity

## Before state

- `caco release status` already accepted `--id`, `--job-id`, and `--release-id`, documented those aliases in help/spec, and emitted the stronger missing-arg message:
  - `--id is required for release status (aliases accepted: --job-id, --release-id). Run caco release list to see queued/active jobs.`
- `caco release logs` still exposed only `--id` in `RELEASE_LOGS_ARGS`.
- `dispatch_release_logs(...)` only read `flags.get("--id")`, so `--job-id` and `--release-id` were treated as unknown flags and then ignored by the dispatcher.
- The logs surface therefore diverged in three ways under one parent bug:
  - aliases omitted from help/spec
  - aliases rejected at dispatch time
  - missing-arg error was terse and lacked the discoverability pointer to `caco release list`

## After state

- `RELEASE_LOGS_ARGS` now declares `--job-id` and `--release-id` alongside canonical `--id`.
- `dispatch_release_logs(...)` now resolves `--id`, `--job-id`, or `--release-id` just like `dispatch_release_status(...)`.
- The missing-arg error for `caco release logs` now matches the stronger status-style template:
  - `--id is required for release logs (aliases accepted: --job-id, --release-id). Run caco release list to see queued/active jobs.`
- The empty-value validator still fires cleanly for alias input too, so `--job-id ''` reaches the same `--id value cannot be empty for release logs ...` guard instead of falling back to a missing-id path.

## Diff summary

- Commit: `bb8552b2e` — `bd-e52dd2: align release logs id aliases`
- Files touched:
  - `crates/caco-cli/src/lib.rs`
  - `crates/caco-cli/src/release_cmd.rs`
- Diff vs current `origin/main`:
  - `crates/caco-cli/src/lib.rs` — +55 lines
  - `crates/caco-cli/src/release_cmd.rs` — +11 / -1 lines
- Behavioural delta:
  - `caco release logs --help` now advertises the same alias affordances as `release status`
  - `caco release logs --job-id ...` and `--release-id ...` are accepted instead of warn-then-ignore
  - missing-id wording now carries the same alias list + discoverability pointer pattern as the sibling surface
- Validation:
  - `cargo build -p caco-cli`
  - `cargo clippy -p caco-cli --all-targets --no-deps -- -D warnings`
  - `cargo test -p caco-cli summary_registry_tests::bd_e52dd2_release_logs_aliases_declared -- --exact --nocapture`
  - `cargo test -p caco-cli summary_registry_tests::bd_e52dd2_release_logs_missing_id_matches_status_template -- --exact --nocapture`
  - `cargo test -p caco-cli summary_registry_tests::bd_e52dd2_release_logs_job_id_alias_reaches_empty_value_validator -- --exact --nocapture`
  - `cargo test -p caco-cli summary_registry_tests::bd_fca3e1_release_status_aliases_declared -- --exact --nocapture`
  - `cargo test -p caco-cli tests::build_test_release_list_validate_project_first -- --exact --nocapture`
  - `cargo test -p caco-cli tests::release_list_validates_channel -- --exact --nocapture`

## Operator-takeaway

This was a small but high-signal CLI polish fix: `release logs` no longer feels like a lesser sibling of `release status`. The three user-visible asymmetries now collapse to one consistent affordance contract, which is exactly the kind of read-only CLI papercut that makes the tool feel more predictable under pressure.
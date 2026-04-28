# Session summary — summaries-list JSON validator envelopes

## Goal

Fix the follow-up JSON-mode validation gap reported after bd-f31814: `caco summaries list --json` should return the same canonical error envelope for local argument validation failures that `caco summaries show --json` now returns.

## Bead(s)

- `bd-bc1792` — [cli] summaries list --json validators should return JSON envelopes

## Before state

- Failing tests: no existing regression for the summaries-list validator path.
- Relevant metrics: reporter observed v1.2.582 returning human stderr and rc=2 for `caco summaries list --limit -1 --json`, `--limit abc --json`, and `--bead-id "" --json`.
- Context: bd-f31814 fixed the sibling `summaries show` validator envelope path, but `summaries list` still returned `CliError` before producing JSON.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: new regression covers invalid `--limit`, invalid `--offset`, empty `--bead-id`, and empty `--agent` under `caco summaries list --json`.
- Context: summaries-list local validators now call the shared `summary_cli_error(...)` helper, returning `{ok:false,error:{code:"invalid_argument",...}}` with exit code 1 in JSON mode while preserving human `CliError` behavior in text mode.

## Diff summary

- Commits: `331ea2701`.
- Files touched: `crates/caco-cli/src/summary_cmd.rs`, `crates/caco-cli/src/lib.rs`.
- Tests: +1 table-driven regression test covering five summaries-list JSON validator failures.
- Behavioural delta: JSON callers now receive a machine-readable envelope instead of human stderr for summaries-list validator failures.
- Validation: `cargo fmt --all -- --check`; queued `cargo test -p caco-cli bd_bc1792 -- --nocapture`; queued `cargo test -p caco-cli bd_f31814 -- --nocapture`.

## Operator-takeaway

The bd-f31814 envelope behavior is now consistent across both `caco summaries show --json` and `caco summaries list --json` for local validation failures.

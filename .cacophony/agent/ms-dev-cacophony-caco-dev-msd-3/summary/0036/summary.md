# Session summary — scratch JSON missing-argument envelopes

## Goal

Make the scratch state-mutating CLI surfaces behave consistently in JSON mode when required arguments are omitted. The work fixes the gap where `caco scratch write`, `append`, `connect`, and `disconnect` returned human stderr with parse-style exit code 2 instead of the structured JSON error envelope expected by JSON callers.

## Bead(s)

- `bd-75f5db` — `caco scratch {write,append,connect,disconnect} --json` missing-required-arg paths emit human stderr instead of JSON envelopes.

## Before state

- Failing tests: no dedicated regression covered JSON-mode missing required scratch arguments for the four mutating scratch commands.
- Relevant metrics: `caco scratch ... --json` omitted-argument paths returned `CliError` directly, producing human output and exit code 2, while empty-value paths already produced `{ ok: false, error: { code, message } }` with exit code 1.
- Context: `scratch_cmd.rs` used direct `flags.get(...).ok_or_else(|| CliError::new(...))?` checks in write, append, connect, and disconnect dispatchers.

## After state

- Failing tests: none observed in the targeted validation run.
- Relevant metrics: JSON-mode missing-argument paths now return `Outcome { exit_code: 1, stdout: cli_error_json("missing_argument", ...) }` for `--note-id`, `--body`, `--text`, `--scope`, and `--target` on the affected commands.
- Context: The implementation is committed locally as `53229bbcf` after rebasing onto the `bd-95cda5` safety-fix mainline, the `bd-f6890c` recovery merge, and the latest `origin/main`, with this file committed as the recorded summary artefact and refreshed after the hold cleared.

## Diff summary

- Commits: `53229bbcf` plus the recorded-summary commit(s).
- Files touched: `crates/caco-cli/src/scratch_cmd.rs`, `crates/caco-cli/src/lib.rs`.
- Tests: added one focused JSON-envelope regression covering all four scratch surfaces; updated existing scratch missing-argument expectations to stay aligned with actionable messages.
- Validation: `cargo test -p caco-cli scratch_missing_required_args_json_emit_envelope_bd_75f5db -- --nocapture`; existing focused scratch usage/actionability tests; `cargo clippy -p caco-cli --all-targets -- -D warnings`; `cargo test-small`.
- Behavioural delta: Human-mode missing-argument messages remain actionable, while JSON-mode callers now receive a structured `missing_argument` envelope on stdout with exit code 1 instead of human stderr and exit code 2.

## Operator-takeaway

The scratch JSON contract is now consistent for omitted required arguments across the mutating note and connection commands; the fix was held during the mainline-safety incident and is now ready for the recorded lifecycle path after the safety-fix mainline closed.

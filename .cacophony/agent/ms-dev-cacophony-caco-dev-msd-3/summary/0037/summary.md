# Session summary — timeline JSON validation envelopes

## Goal

Fix the timeline CLI sibling of the scratch JSON-envelope gap: when `caco timeline --json` rejects invalid user input or receives a daemon HTTP error, programmatic callers should get a structured JSON error on stdout with a non-zero command outcome rather than human stderr or raw daemon blobs.

## Bead(s)

- `bd-b31c82` — `caco timeline --json` validation errors emit human stderr.

## Before state

- Failing tests: no dedicated regression covered timeline JSON-mode validation failures.
- Relevant metrics: helsinki v1.2.570 repro reported `--max-age-hours` invalid, `--limit` out-of-range, unknown `--scope`, `--scope=project` missing `--project`, and unknown-project HTTP 404 returning human stderr / rc=2 or raw daemon JSON snippets instead of canonical JSON envelopes.
- Context: `dispatch_timeline` returned `CliError` directly for validation and daemon HTTP errors even when the caller requested `--json`.

## After state

- Failing tests: none observed in validation.
- Relevant metrics: CLI-side timeline validation failures now route through `bd_cli_error(..., "invalid_argument", ...)` in JSON mode, producing `{ ok: false, error: { code: "invalid_argument", message } }` with exit code 1. Timeline HTTP errors are also condensed into concise `daemon_error` messages instead of leaking the daemon URL plus raw JSON body.
- Context: The implementation is committed locally as `d7fbf70c6` after rebasing onto the latest `origin/main`, and is ready for recorded lifecycle reintegration once board and mainline checks are healthy.

## Diff summary

- Commits: `d7fbf70c6` plus this recorded-summary commit.
- Files touched: `crates/caco-cli/src/lib.rs`.
- Tests: added focused regression coverage for JSON envelopes across invalid `--max-age-hours`, out-of-range `--limit`, unknown `--scope`, and missing project under `--scope=project`; added a unit guard that HTTP daemon errors are concise and classified as `daemon_error`.
- Validation: `cargo test -p caco-cli timeline_json_validation_errors_emit_envelope_bd_b31c82 -- --nocapture`; `cargo test -p caco-cli timeline_http_errors_are_concise_daemon_errors_bd_b31c82 -- --nocapture`; `cargo clippy -p caco-cli --all-targets -- -D warnings`; `cargo test-small`.
- Behavioural delta: `caco timeline --json` validation errors now match the structured error-envelope convention already used by `caco bd create --json`, `caco msg send --json`, and the bd-75f5db scratch fix.

## Operator-takeaway

Timeline is no longer a separate JSON-contract drift point for common validation failures; scratch and timeline now share the same programmatic error-envelope behavior for the reported validator paths.

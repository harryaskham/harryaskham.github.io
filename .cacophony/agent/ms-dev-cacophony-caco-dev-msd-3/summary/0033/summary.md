# Session summary — strict global JSON boolean parsing

## Goal

Fix the CLI regression where `--json=<value>` was treated as an ordinary command flag instead of the global JSON mode selector, and keep validation green by separately tracking and fixing the pre-existing `caco-cli` clippy failures found during this bead's preflight.

## Bead(s)

- `bd-7c239f` — `caco --json=` boolean equals-form silently falls through instead of being accepted or rejected.
- `bd-dffaba` — `[broken-on-main] caco-cli clippy rejects audio live args and SpeechConfig fixture`.

## Before state

- Failing tests: focused parser coverage for `--json=true`, `--json=bogus`, and `--json bogus` was missing; `cargo clippy -p caco-cli --all-targets -- -D warnings` failed on unrelated mainline clippy lints.
- Relevant metrics: `--json=true` on normal commands could silently produce human output; `--json=bogus` did not reject.
- Context: `bd-7c239f` was claimed to close the gap left by the earlier `bd-d351f8` boolean-space-form support. During validation, the caco-cli clippy run exposed `bd-dffaba`, which was filed and claimed separately.

## After state

- Failing tests: none observed in the validation run.
- Relevant metrics: 4 focused `bd-7c239f` unit tests pass; `cargo clippy -p caco-cli --all-targets -- -D warnings` passes; `cargo test-small` passes.
- Context: The parser now recognizes `--json=true/false/1/0/yes/no`, rejects invalid or empty values for both equals and value-like space forms, and returns a structured JSON error envelope when the invalid value was intended to request JSON output.

## Diff summary

- Commits: `6c1a9277c` (`bd-dffaba: fix caco-cli clippy regressions`), `9206fb105` (`bd-7c239f: validate global json boolean values`).
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/audio_cmd.rs`.
- Tests: +4 focused parser/structured-error tests; no tests removed.
- Behavioural delta: `--json=<bool>` is now a real global flag form instead of an ignored command flag, and invalid boolean spellings produce an actionable `invalid_argument` envelope in JSON-intended mode. The unrelated clippy regressions are fixed with a scoped allow on the live audio dispatcher and an initializer-style test fixture.

## Operator-takeaway

Machine callers can now use getopt-style `--json=true` safely and will get a clear rejection for bogus JSON flag values instead of silent human output. The validation-discovered clippy failures were handled under their own bead (`bd-dffaba`) so the parser fix and broken-on-main cleanup remain traceable even though they land in the same reintegration batch.

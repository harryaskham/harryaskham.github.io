# Session summary — Remediation status dispatch

## Goal

Fix the `caco remediation status` CLI path so the operator-facing diagnostics command executes its read-only status renderer instead of falling back to static help/metadata output.

## Bead(s)

- `bd-2a05f3` — caco remediation status renders help instead of diagnostics

## Before state

- Failing tests: no pre-existing regression test for the dispatcher bug.
- Relevant metrics: `caco remediation status --json` could return command metadata (`command`, `summary`, `args`, `mcp`) instead of diagnostic data.
- Context: remediation command metadata was present, but the main CLI dispatcher had no concrete `remediation status` arm.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: `cargo check -p caco-cli` passed; `cargo test -p caco-cli remediation_status_dispatches_diagnostics_instead_of_help_bd_2a05f3` passed.
- Context: `caco remediation status` now dispatches to the diagnostics renderer and `--json` returns a diagnostic payload with `ok`, `data.rows`, and `data.count`.

## Diff summary

- Code/content commits: `3dc806f2b`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +1 / -0 / flipped 0
- Behavioural delta: the `remediation status` command path is executable and no longer renders help metadata for the status subcommand.

## Operator-takeaway

This was a wiring bug: the command tree advertised remediation diagnostics before the dispatcher executed it. The fix is intentionally narrow and regression-tested so future metadata-only command additions do not silently look functional while returning help.

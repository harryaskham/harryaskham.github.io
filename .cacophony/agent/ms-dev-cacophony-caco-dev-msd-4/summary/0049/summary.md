# Session summary — default project command resolution

## Goal

Fix project-scoped CLI commands so omitting `--project` honors the configured top-level `default_project` before falling back to the first configured project.

## Bead(s)

- `bd-b43a92` — Update commands to respect default_project setting

## Before state

- Failing tests: new regression tests initially failed until the temporary test config included required `nodes` and `services` fields.
- Relevant metrics: `resolve_project` used `--project`, `CACO_PROJECT`, `CACOPHONY_PROJECT`, then first configured project; this made commands such as `caco bd stats` ignore `default_project`.
- Context: interactive runtime commands had a deliberate exception to avoid silently using `default_project`, and that exception needed to remain intact.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `resolve_project` now resolves `--project`, `CACO_PROJECT`, `CACOPHONY_PROJECT`, `default_project`, then first configured project. A stale `default_project` now errors loudly instead of falling through.
- Context: CLI help, README, AGENTS, and SPEC now document the resolution order and the interactive-runtime exception.

## Diff summary

- Commits: `54f6006c6`
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-cli resolve_project --lib`; `git diff --check`
- Behavioural delta: project-scoped commands using the shared resolver now honor top-level `default_project`; interactive shorthand commands still do not.

## Operator-takeaway

Commands like `caco bd stats` should now target the operator-configured default project when no explicit project/env override is present, removing the surprising alphabetically/declared-first fallback for normal project-scoped command flows.

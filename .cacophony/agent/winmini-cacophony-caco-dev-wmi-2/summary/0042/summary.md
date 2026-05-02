# Session summary — interactive pi shorthand respects explicit @node:project targets

## Goal

Fix the interactive Pi shorthand so an explicitly provided `@node:project` target actually counts as project selection, and clean up the matching missing-project error so users see only the supported ways to provide project context instead of internal implementation notes.

## Bead(s)

- `bd-b3e637` — Fix --project requirement for interactive pi spawns
- `bd-f3710c` — Improve error messaging for missing --project in pi

## Before state

- The `@`-target parser already understood `@node:project` and preserved the project component.
- But `dispatch_at_target(...)` only injected that parsed project into tokens for `tui`; it ignored the same project for interactive runtimes like `pi`.
- Result: `caco pi @host:project` still fell through to `resolve_interactive_project(...)` as if no project had been specified.
- The fallback error from `resolve_interactive_project(...)` also mentioned internal design details (`bd-4896e6`, `default_project`, `sole-configured-project`) that were useful to implementers but noisy for users.

## After state

- Extracted the `@`-target qualifier forwarding into `inject_at_target_project_and_view(...)`.
- `@node:project` now seeds `--project` for:
  - `pi`
  - `claude`
  - `codex`
  - `tui` (existing behaviour preserved)
- `view` forwarding remains `tui`-only.
- The missing-project error for interactive runtime shorthands now clearly lists only the supported sources:
  - `--project`
  - `CACO_PROJECT`
  - `CACOPHONY_PROJECT`
  - `interactive_defaults.<runtime>.project`
- The error no longer leaks internal policy/bead language into the user-facing message.

## Diff summary

- Commit: `0fbc26dee` — `bd-b3e637 bd-f3710c: improve interactive pi project handling`
- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Tests added/updated:
  - `tests::at_target_project_is_forwarded_to_pi_shorthand_bd_b3e637`
  - strengthened `tests::pi_requires_project_flag`
- Validation:
  - `cargo test -p caco-cli tests::at_target_project_is_forwarded_to_pi_shorthand_bd_b3e637 -- --exact --nocapture`
  - `cargo test -p caco-cli tests::pi_requires_project_flag -- --exact --nocapture`
  - `cargo build -p caco-cli`
  - `cargo clippy -p caco-cli --all-targets --no-deps -- -D warnings`

## Operator-takeaway

This batch fixed a genuine mismatch between the parser and the interactive runtime launcher: the CLI already knew the project from `@node:project`, but the Pi shorthand didn’t reuse it. Now the explicit target form behaves like users expect, and the remaining error path is shorter and clearer when no project source is present.
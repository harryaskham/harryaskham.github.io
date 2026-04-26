# Session summary — short msg send targets

## Goal

Make `caco msg send --target` friendlier for operators and agents who copy a short agent ID from bead tables or status output by expanding an unambiguous current-project short ID before sending.

## Bead(s)

- `bd-436558` — Accept unambiguous short agent ids in caco msg send target

## Before state

- Failing tests: none.
- Relevant metrics: `caco msg send --target <short-agent-id>` could fail even when the short ID uniquely identified a current-project agent; users had to manually rewrite it into a project-qualified target.
- Context: `--strict-target` already queried project agents, but ordinary send did not use that inventory to canonicalize short targets.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: unambiguous short agent IDs now expand to the stable project-qualified or fully-qualified target returned by project agent inventory; qualified and ambiguous targets are preserved.
- Context: `--cc` fallback targets use the same aliasing, `--strict-target` accepts the same unambiguous short form, and SPEC / messaging docs describe the behavior.

## Diff summary

- Commits: `e770c92db`
- Files touched: `crates/caco-cli/src/msg_cmd.rs`, `crates/caco-cli/src/lib.rs`, `SPEC.md`, `docs/messaging.html`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-cli bd436558 --lib`; `docs/validate-pages.sh`; `git diff --check`
- Behavioural delta: `caco msg send` resolves unambiguous short current-project agent IDs before constructing the daemon request; ambiguous short IDs are not guessed.

## Operator-takeaway

Copying a short agent ID from common Cacophony tables into `caco msg send --target` is now expected to work when the current project makes that ID unambiguous, reducing coordination friction without weakening target safety.

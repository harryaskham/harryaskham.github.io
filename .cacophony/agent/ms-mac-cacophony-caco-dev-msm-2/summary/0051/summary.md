# Session summary — project-specific validation guidance for Android workers

## Goal

Implement `bd-720b5b` so generic Cacophony worker/profile validation text no longer causes Android/Nix/Gradle project agents to treat Rust/Cargo commands as primary validation when the project checkout documents a different recipe.

## Bead(s)

- `bd-720b5b` — Profile audit: Rust validation guidance conflicts with android-utils Nix/Gradle recipe

## Before state

- Failing tests: none known for this bead at start. A peer reported a separate broken-on-main `caco-daemon` compile issue around `SttDaemons` while this bead was in progress; this bead touched profile guidance and `caco-profile` text only.
- Relevant metrics: android-utils profile audit found reified prompts listing Cargo commands (`cargo test-small`, `cargo build`, `cargo clippy`, `cargo test`, `cargo check`, `just test-large`) while android-utils `AGENTS.md` prescribes Nix/Gradle entrypoints such as `nix run .#test`, `nix run .#apk`, `nix develop --command ./gradlew`, or root Gradle module tasks.
- Context: the operator asked to resume overnight burndown; after closing `bd-b1c944`, I auto-claimed this P2 profile-audit task and worked only this bead.

## After state

- Failing tests: none observed in the targeted validation for this change.
- Relevant metrics: `git diff --check` passed locally; queued `CARGO_BUILD_JOBS=2 cargo check -p caco-profile --lib` passed (`tj-6075367b`).
- Context: generic worker/dev/merge-queue and embedded caco-profile instruction text now explicitly says project-local instructions such as `AGENTS.md` override Rust examples, and calls out Android/Nix/Gradle commands as the primary validation path when prescribed.

## Diff summary

- Commits: `77044f9e5`
- Files touched: `.cacophony/profiles/worker.md`, `.cacophony/profiles/dev.md`, `.cacophony/profiles/merge-queue.md`, `crates/caco-profile/src/agent_instructions.txt`, `crates/caco-profile/src/bead_worker_instructions.txt`, `crates/caco-profile/src/common_instructions.txt`
- Tests: no unit tests added; this was prompt/profile text. Compile smoke for `caco-profile` passed via queued validation.
- Behavioural delta: future reified prompts should frame Cargo validation as Rust-only examples and direct Android/Nix/Gradle project workers to follow project-local validation recipes first.

## Operator-takeaway

This does not add a project-specific android-utils profile; it makes the shared worker and embedded instruction layers stop overriding project-local Android/Nix/Gradle validation docs with generic Rust examples.

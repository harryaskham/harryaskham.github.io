# Session summary — interactive runtime shorthands infer project from cwd remotes

## Goal

Implement Harry's request that interactive commands like `caco pi` infer the managed project from the current git repository when no explicit project or project environment is present. This avoids creating orphaned visiting Pi agents when the operator runs `caco pi` from his own clone of a configured hosted repository that lacks an `.envrc` `CACO_PROJECT`; the session also repaired pre-existing clippy breakage that blocked the requested foreground validation path.

## Bead(s)

- `bd-d97dff` — Infer interactive command project from cwd git remote
- `bd-be2762` — [broken-on-main] caco-cli/caco-daemon clippy -D warnings failing

## Before state

- Failing tests: none known for the project resolver before the change. During revived-session validation, `cargo clippy -p caco-cli --lib -- -D warnings` exposed pre-existing clippy failures in caco-daemon, caco-cli, and caco-tui code outside the bd-d97dff feature diff.
- Relevant metrics: one feature bead filed from the operator request and claimed by `winmini-cacophony-caco-dev-wmi-2`; one broken-on-main clippy bead filed and claimed after dedup searches found no existing tracker.
- Context: `resolve_interactive_project` accepted `--project`, `CACO_PROJECT` / `CACOPHONY_PROJECT`, or `interactive_defaults.<runtime>.project`. If `caco pi` lacked those, it fell into visiting-agent mode and derived a dynamic project from the current directory, even when the cwd git remote uniquely matched a configured Cacophony project.

## After state

- Failing tests: none in focused foreground validation.
- Relevant metrics: three focused caco-cli tests cover remote matching, ambiguity, and common GitHub remote URL forms; five clippy warnings blocking caco-cli validation were fixed, plus one additional caco-tui warning discovered after clippy progressed.
- Context: interactive project resolution now tries cwd `git remote -v` against configured project remotes after explicit flag/env and before interactive defaults. It is disabled inside managed-agent environments (`CACO_AGENT_ID` / `CACOPHONY_AGENT` / `CACO_DEV_DIR` presence via the existing visiting-agent guard) so worker checkouts are not reinterpreted as operator-owned cwd launches. `caco pi` only uses visiting-agent mode when no managed project resolves.

## Diff summary

- Code/content commits: `3e117f47d` (`bd-d97dff: infer interactive project from git remote`), `bceb4fc7b` (`bd-be2762: fix clippy warnings blocking cli validation`).
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/clippy_diagnostic_capture.rs`, `crates/caco-daemon/src/decision_points.rs`, `crates/caco-daemon/src/lib.rs`, `crates/caco-tui/src/kitty.rs`, `SPEC.md`, `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/pending/summary.md`.
- Tests: +3 focused CLI unit tests; no tests removed or flipped.
- Validation:
  - `git diff --check`.
  - `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib bd_d97dff` passed after the revived-session rerun.
  - `cargo build -p caco-cli` passed.
  - `cargo clippy -p caco-cli --lib -- -D warnings` initially exposed pre-existing broken-on-main lint failures, then passed after the `bd-be2762` fixes.
  - `./scripts/rustfmt-changed.sh` formatted changed clean files and intentionally skipped pre-existing non-rustfmt-clean HEAD files `crates/caco-daemon/src/decision_points.rs` and `crates/caco-daemon/src/lib.rs` to avoid unrelated churn.
- Behavioural delta: `caco pi` / `caco claude` / `caco codex` can resolve the target managed project from a unique cwd git remote match, and `caco pi` avoids visiting mode for those ordinary operator clone launches. The caco-cli clippy validation path also no longer trips over the repaired pre-existing lint warnings.

## Operator-takeaway

The important shift is that the operator's current repo clone now participates in project selection for interactive runtime shorthands. Explicit flags and env still win, ambiguous remote matches fail closed, managed agents are excluded so this does not disturb worker lifecycle commands, and the revived session left the touched CLI validation lane clippy-clean.

# Session summary — previous-summary docs leakage guard

## Goal

Prevent runtime-local previous-summary context injected into managed agent checkouts from being accidentally committed into stable repository documentation such as `CLAUDE.md` or `AGENTS.md`.

## Bead(s)

- `bd-48665f` — [docs] previous-summary injection should not persist stale agent-local block in CLAUDE.md

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: no reintegration-time guard existed for committed `<!-- BEGIN AUTOGEN: previous-summaries (bd-20d2dc) -->` blocks in tracked docs.
- Context: A documentation audit had found a stale agent-local previous-summaries block in mainline `CLAUDE.md`, showing that checkout-local startup context could be committed by accident.

## After state

- Failing tests: none observed.
- Relevant metrics: new focused daemon regression tests cover committed-block refusal and uncommitted runtime-block tolerance.
- Context: `reintegrate(...)` now refuses to proceed if `HEAD:CLAUDE.md` or `HEAD:AGENTS.md` contains the previous-summaries AUTOGEN sentinel, while leaving uncommitted runtime injection to the normal clean-worktree checks.

## Diff summary

- Commits: `14381c351`.
- Files touched: `crates/caco-daemon/src/reintegration.rs`.
- Tests: +2 daemon unit tests / -0 / flipped 0.
- Behavioural delta: Reintegration now checks tracked docs with `git grep` before any merge/PR/artifact path, and reports an actionable `bd-48665f` error instructing the worker to remove the runtime-local AUTOGEN block before retrying.
- Validation: `cargo fmt --all`; `cargo test -p caco-daemon previous_summaries_guard -- --nocapture`; `cargo clippy -p caco-daemon --all-targets -- -D warnings`; `cargo test-small`.

## Operator-takeaway

The previous-summary injection can still help agents at runtime, but a worker can no longer land that per-agent startup context into the repository’s canonical docs unnoticed.

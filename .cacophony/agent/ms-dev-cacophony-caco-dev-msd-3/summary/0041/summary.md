# Session summary — stale recorded-summary commit reference guard

## Goal

Add a recorded-summary validation guard so frequent rebases do not leave `summary.md` pointing at stale pre-rebase commit SHAs. This directly addresses the workflow friction where agents create safe backup branches and rebase often, but must manually remember to refresh commit references before recorded reintegration.

## Bead(s)

- `bd-d3116d` — Recorded summaries need a stale-commit refresh check after rebase

## Before state

- Failing tests: no focused validation caught stale `- Commits: `SHA`` entries in recorded summaries after a rebase.
- Relevant metrics: focused `cargo test -p caco-daemon bd_d3116d -- --nocapture` passed; `cargo clippy -p caco-daemon --all-targets -- -D warnings` passed; `git diff --check` passed.
- Context: `validate_recorded_summary` checked required sections and whether the summary was touched in the current range, but a summary could still mention an old commit SHA that was rewritten by rebase and no longer an ancestor of `HEAD`.

## After state

- Failing tests: none for the focused bd-d3116d validation.
- Relevant metrics: +2 daemon unit tests for stale/current summary commit references.
- Context: recorded summary validation now scans `Commits:` / `- Commits:` lines for backtick-wrapped hexadecimal commit references and rejects any that do not exist or are not ancestors of current `HEAD`, with an explicit bd-d3116d message telling the agent to refresh the summary after rebase.

## Diff summary

- Commits: `ded9c64ff` (`bd-d3116d: reject stale summary commit references`)
- Files touched: `crates/caco-daemon/src/reintegration.rs`
- Tests: +2 / -0 / flipped 0
- Behavioural delta: recorded reintegration now fails early when summary commit references are stale after rebase, preventing durable summary artefacts from recording obsolete pre-rebase SHAs.

## Operator-takeaway

Frequent safe rebasing is now less error-prone: if an agent forgets to refresh commit hashes in its recorded summary, the daemon will stop the reintegration before publishing the stale artefact.

# Session summary — Helsinki beads snapshot disk pressure source fix

## Goal

Address the source-side cause of P0 `bd-d4c459`: high-rate destructive-rewrite forensic snapshots under `.beads/snapshots/` were consuming helsinki disk space. This chunk adds a code-level rate limit and keeps destructive cleanup of existing snapshots gated on the operator choice.

## Bead(s)

- `bd-d4c459` — `[disk] helsinki beads snapshots consume 170G and disk free space is falling`

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: helsinki log-monitor reported `.beads/snapshots` around 170–172G, filesystem around 95% used, and continuing snapshot growth. First-party dry-run `caco @node:helsinki bd snapshot rotate --project cacophony --retention-days 1 --dry-run` estimated deletion of 5194 unpinned snapshots and 129,163,440,417 bytes freed while preserving 1885 recent snapshots.
- Context: an operator choice `choice-019dd31b-3b4e-7e91-b765-7f2efb8915d8` is active and gates destructive cleanup. No deletion was run in this chunk.

## After state

- Failing tests: none observed.
- Relevant metrics: queued `cargo test -p caco-beads snapshot_before_destructive_write_is_rate_limited_bd_d4c459 --lib` passed in job `tj-9a6f42eb`; queued `cargo test -p caco-cli agent_rebase --lib` passed in job `tj-6ab7bc19`; `docs/validate-pages.sh` passed with 1861 checks; `cargo fmt --all -- --check` and `git diff --check` passed.
- Context: automatic destructive-rewrite snapshot creation now reuses the most recent snapshot when it is younger than 15 minutes, preventing multiple full snapshots per minute. Existing helsinki cleanup remains blocked on the active operator choice.

## Diff summary

- Commits: `4c581a18f` (`bd-d4c459: rate-limit beads snapshots`), `ad6ec8c2b` (session summary)
- Files touched: `crates/caco-beads/src/store.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/beads.html`
- Tests: +1 focused caco-beads unit test covering rate-limited snapshot reuse; existing caco-cli agent-rebase tests were rerun to guard prior branch changes after rebase.
- Behavioural delta: `snapshot_before_destructive_write` now creates at most one automatic forensic snapshot per project per 15-minute window, while preserving the existing first snapshot contents and leaving first-party `caco bd snapshot rotate --dry-run` as the visible cleanup path.

## Operator-takeaway

This landed the prevention half of the helsinki disk incident: new builds should stop the high-rate snapshot explosion. The existing 170G+ backlog still requires explicit operator-approved first-party cleanup before `bd-d4c459` can be fully closed.

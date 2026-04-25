# Session summary — bd-6f7685 TUI summaries detail retry guidance

## Goal

Improve the TUI session summaries view so a selected summary whose detail fails to load shows the actual failure and a clear retry path instead of falling back to generic selection copy.

## Bead(s)

- `bd-6f7685` — TUI summaries: show retry guidance on failed detail loads

## Before state

- The summaries list could load successfully while the selected detail request failed.
- In that state, the detail pane did not surface the per-detail error path clearly; operators could see generic body-loading guidance rather than the actionable failure.
- Acceptance requested `cargo test -p caco-tui summaries --lib` and `cargo test-small`.

## After state

- The detail pane now checks `summary_detail_error` when the selected detail is not loading and no matching detail body is present.
- Failed detail loads render a red failure heading, the daemon error text, `r` refresh/retry guidance, and a follow-up filing hint if the failure persists.
- Added a regression test rendering the summaries view with a detail-load error and asserting the error plus retry guidance appear.

## Diff summary

- Commit: `bda54d47f` after replay onto the remote agent branch.
- Files touched: `crates/caco-tui/src/views/summaries.rs`.
- Tests: `cargo test -p caco-tui summaries --lib`; `cargo test-small`; `cargo fmt --all -- --check`; `git diff --check`.
- Behavioural delta: the TUI summaries detail pane now makes per-summary load failures explicit and recoverable.

## Operator-takeaway

A summaries list/detail partial failure is no longer ambiguous: the TUI now tells the operator the selected detail failed, shows the underlying error, and points them to refresh/retry before filing follow-up.

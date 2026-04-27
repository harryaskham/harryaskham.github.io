# Session summary — direct recorded reintegration refused remote agent branch divergence

## Goal

Complete the operator-requested landing attempt for `bd-771b58`: after preserving a fallback branch, rebasing onto current `origin/main`, and validating the caco-web Workspace fix, submit direct recorded reintegration so the work can land.

## Bead(s)

- `bd-771b58` — caco-web Workspace narrow agent pane table overflows horizontally. Rebased and validated locally, but still unlanded after direct recorded reintegration refused to overwrite the remote agent branch.
- `bd-378dde` — direct recorded reintegration recovery should handle same-content remote agent branch divergence. Related active P0 tracker; the direct attempt failed with the same `bd-4b1ffd` guard family, though this remote branch includes at least one unrelated TUI commit.
- `bd-95cda5` — direct-recorded partial-publish recurrence tracker. Still relevant background for the operator-approved direct attempt.

## Before state

- Failing tests: none from the caco-web targeted validation. `CARGO_BUILD_JOBS=2 cargo test -p caco-web workspace_agents_table_collapses_secondary_columns_on_mobile_bd_771b58 --lib` passed; `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets` passed.
- Relevant metrics: branch was successfully rebased onto `origin/main` and was `ahead 44` before the reintegration attempt.
- Context: fallback branch `preserve/ms-mac-cacophony-caco-web-pre-direct-reintegrate-20260427-145110` preserves the pre-rebase state.

## After state

- Failing tests: none newly observed; reintegration failed before landing.
- Relevant metrics: branch remains `ahead 44` of `origin/main`; `bd-771b58` remains unclosed/unlanded. Remote agent branch `origin/agent/ms-mac/cacophony/ms-mac-cacophony-caco-web` contains remote-only commits not in local HEAD or `origin/main`.
- Context: `caco agent reintegrate --id ms-mac-cacophony-caco-web --mode direct,recorded` refused with `bd-4b1ffd` rather than overwriting the remote agent branch. Recent remote-only commits include `faa8283dc` (`chore(caco-web): record dashboard observation cycle`) and `ddda27079` (`Reintegrate agent branch agent/ms-mac/cacophony/ms-mac-cacophony-caco-tui`).

## Diff summary

- Commits: no new product-code commit after the rebased validation summary; this summary records the failed reintegration attempt.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0077/summary.md` and `web/direct-reintegration-attempt.log`.
- Tests: no additional tests after the successful validation in `0076`.
- Behavioural delta: none landed. The direct recorded path refused before publishing because it detected remote-only agent-branch commits that could be unknown unlanded work.

## Embedded artefacts

- `web/direct-reintegration-attempt.log` — command, failure text, git status, remote-only commits, and local tip after the refused direct recorded reintegration.

## Operator-takeaway

The operator-approved direct recorded attempt was made after rebase and validation, and it failed safely before landing due the active remote-agent-branch divergence guard. `bd-771b58` is still ready locally, but landing now needs either the `bd-378dde` recovery path, cleanup/reconciliation of the remote agent branch by the appropriate owner, or an explicitly approved alternate landing path that does not overwrite unrelated remote-only work.

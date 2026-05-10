# Session summary — TUI attach env order and realtime declaration docs

## Goal

Run a technical-writer review pass: check inbox, audit recent first-parent mainline commits, update drifted repository and Pages documentation, validate the docs site, and reintegrate the docs-only update.

## Bead(s)

- `bd-dfe7de` — TUI replace-command buffer preallocation preserves exact retained-placement bytes.
- rapid config cleanup — removed the checked-in realtime controller persistent declaration while leaving realtime experiments manually composable.
- TUI attach fix — remote tmux attach command construction now puts `env -u` options before the `LANG` assignment.

## Before state

- Failing tests: none known for the documentation lane.
- Relevant metrics: checkout was clean at `2f39067b3`; `origin/main` advanced through `63be04b57` with three first-parent commits.
- Context: inbox contained ms-dev GitHub-over-443 / remote-config drift broadcasts; those are ms-dev scoped and did not block this beelink technical-writer pass.

## After state

- Failing tests: none in docs validation.
- Relevant metrics: `docs/daily-changelog.md` now covers 57 non-empty days and 8589 first-parent mainline commits through `63be04b57`. `./docs/validate-pages.sh` reported 3313 passed, 0 warnings, 0 failed; `git diff --check` was clean.
- Context: AGENTS/configuration docs no longer imply a checked-in persistent `rt-ctrl` exists by default, TUI docs describe the `env -u` ordering for remote attach commands, and the daily changelog includes the new May 11 entries.

## Diff summary

- Commits: pending direct reintegration docs commit.
- Files touched: `AGENTS.md`, `docs/configuration.html`, `docs/tui.html`, `docs/daily-changelog.md`, and this summary.
- Tests: +0 / -0 / flipped 0; documentation validation only.
- Behavioural delta: Documentation now reflects the current persistent realtime declaration state, the fixed remote tmux attach command shape, and the latest TUI allocation work without changing runtime behavior.

## Operator-takeaway

Realtime controller experiments remain available through the realtime profile/snippet, but there is no default checked-in `rt-ctrl` persistent declaration; remote agent attach commands now use `env -u ... LANG=...` ordering so UTF-8 setup no longer breaks `env` parsing.

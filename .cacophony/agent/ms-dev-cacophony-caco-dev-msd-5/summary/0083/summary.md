# Session summary — bd-cc952f tracked edtui readonly viewer fallback

## Goal

Address `bd-cc952f`: replace untracked edtui readonly-detail TODO comments in TUI crons/hooks views with explicit tracked fallback comments and focused tests, preserving current read-only behavior.

## Changes

- Replaced the crons script-body TODO with a `bd-cc952f`-linked tracked fallback comment.
- Replaced the hooks script-body TODO with the same `bd-cc952f`-linked tracked fallback comment.
- Added source-contract tests in both modules asserting the fallback is explicitly tracked and the old untracked TODO text does not reappear.
- No rendering behavior changed; the existing readonly syntax-highlighted script detail pane remains in place.

## Validation

- `cargo test -p caco-tui --lib cc952f -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `6483b4f719`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.

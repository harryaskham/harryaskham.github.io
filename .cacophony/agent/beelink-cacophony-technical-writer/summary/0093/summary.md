# Session summary — TUI benchmark raw-ratio warning docs

## Goal

Run the technical-writer review pass, audit recent commits, and keep operator-facing docs plus GitHub Pages aligned with the latest TUI benchmark warning and comparison-summary behavior.

## Bead(s)

- `bd-f77031` — Reuse pending-cleanup state in TUI upload-pass fast paths
- `bd-163d63` — Reuse precomputed cleanup in upload-pass gating
- `bd-ef21ff` — Avoid cloning background cache key vectors on hits
- `bd-736f43` — Centralize background-cache hit lookup
- `bd-0ae0dc` — Borrow cached background entries without helper allocation
- `bd-7f178b` — Warn when terminal-inclusive TUI FPS is materially below raw app-work FPS

## Before state

- Failing tests: none known.
- Relevant metrics: previous Pages validation was clean.
- Context: Recent TUI commits refined background-cache hit handling and benchmark warnings. Existing docs already covered terminal-inclusive fields, but did not mention the new material terminal-inclusive-vs-raw app-work warning or compare-summary raw-ratio reporting.

## After state

- Failing tests: none observed.
- Relevant metrics: `./docs/validate-pages.sh` passed with `3313 passed, 0 warnings, 0 failed`.
- Context: `README.md`, `AGENTS.md`, and `docs/tui.html` now document that benchmark wrappers warn when terminal-inclusive FPS is materially below raw app-work FPS, and that compare summaries preserve the raw app-work ratio alongside the terminal-inclusive ratio.

## Diff summary

- Commits: `56b9dfda3`
- Files touched: `README.md`, `AGENTS.md`, `docs/tui.html`
- Tests: documentation validation only; `./docs/validate-pages.sh` passed.
- Behavioural delta: no runtime behavior changed; docs now match current TUI benchmark warning and comparison summary semantics.

## Operator-takeaway

When terminal sync shows Ghostty/Kitty command processing costs, TUI benchmark docs now tell operators to compare the terminal-inclusive ratio against the raw app-work ratio instead of treating a single work-FPS number as the whole story.

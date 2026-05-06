# Session summary — TUI benchmark terminal-sync default docs

## Goal

Run the technical-writer review pass, audit recent commits, and keep operator-facing docs plus GitHub Pages aligned with the latest TUI benchmark harness behavior.

## Bead(s)

- `bd-797d0c` — Record app-frame time before terminal sync in TUI benchmark samples
- `bd-013923` — Default terminal sync for graphics runs in `scripts/tui-fps-bench.sh`
- `bd-a23be5` — Cache title decoration keys with border segment keys
- `bd-2aa972` — Warn when terminal-inclusive TUI FPS is materially below app-work FPS
- `bd-31b35f` — Clone pending upload payloads after the upload budget is applied
- `bd-4a858e` — Defer changed payload hashing until upload/retention paths need it
- `bd-19852e` — Preserve raw app-work ratio alongside terminal-inclusive comparison ratio

## Before state

- Failing tests: none known.
- Relevant metrics: previous Pages validation was clean.
- Context: Recent TUI commits made graphics benchmark runs default to terminal sync, recorded whether that sync defaulted, and clarified that app-frame samples are captured before the terminal-sync barrier. Existing docs covered terminal-inclusive fields but did not mention the graphics-run default, `--no-terminal-sync`, `harness.terminal_sync_defaulted`, or the no-double-counting measurement shape.

## After state

- Failing tests: none observed.
- Relevant metrics: `./docs/validate-pages.sh` passed with `3313 passed, 0 warnings, 0 failed`.
- Context: `README.md`, `AGENTS.md`, and `docs/tui.html` now document the graphics-run terminal-sync default, explicit `--no-terminal-sync` escape hatch, harness metadata for defaulted sync, and app-frame-plus-terminal-sync accounting.

## Diff summary

- Commits: `0848e431b`
- Files touched: `README.md`, `AGENTS.md`, `docs/tui.html`
- Tests: documentation validation only; `./docs/validate-pages.sh` passed.
- Behavioural delta: no runtime behavior changed; docs now match current TUI benchmark harness defaults and evidence fields.

## Operator-takeaway

TUI graphics benchmark runs now include terminal-side processing by default; operators can still opt out with `--no-terminal-sync`, and the JSON records when the wrapper made that choice automatically.

# Session summary — TUI retained payload byte docs

## Goal

Run the technical-writer review pass, audit recent commits, and keep the TUI benchmark/cache documentation aligned with the latest retained-image accounting behavior.

## Bead(s)

- `bd-5f4eb5` — Avoid double-counting shared retained Kitty payload bytes

## Before state

- Failing tests: none known.
- Relevant metrics: previous Pages validation was clean.
- Context: Recent TUI work changed retained-image accounting so a logical surface aliasing a shared retained payload still tracks placement/delete state locally but does not count the same terminal-retained payload bytes again. The benchmark docs did not call out that retained-byte totals are de-duplicated for shared retained payload aliases.

## After state

- Failing tests: none observed.
- Relevant metrics: `./docs/validate-pages.sh` passed with `3313 passed, 0 warnings, 0 failed`.
- Context: `docs/tui.html` now documents that shared retained-payload aliases are not double-counted in retained-byte totals.

## Diff summary

- Commits: `72e27a387`
- Files touched: `docs/tui.html`
- Tests: documentation validation only; `./docs/validate-pages.sh` passed.
- Behavioural delta: no runtime behavior changed; Pages now matches retained-image byte accounting for shared payload aliases.

## Operator-takeaway

When interpreting TUI graphics telemetry, retained redisplays remain distinct from uploads and retained-only display commands still have wire cost, but shared retained payload aliases should not inflate retained-byte totals.

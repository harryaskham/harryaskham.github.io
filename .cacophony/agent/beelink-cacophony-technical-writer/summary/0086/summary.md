# Session summary — TUI retained redisplay wire-byte docs

## Goal

Run the technical-writer review pass, audit recent implementation commits for documentation drift, and keep the operator-facing TUI benchmark documentation aligned with the current retained-image telemetry behavior.

## Bead(s)

- `bd-f0fdbb` — Report retained-only TUI Kitty wire bytes in benchmark telemetry

## Before state

- Failing tests: none known.
- Relevant metrics: `./docs/validate-pages.sh` had passed on the previous idle pass.
- Context: Recent TUI commits changed how retained-only Kitty redisplays contribute to graphics telemetry; docs already separated retained redisplays from full uploads but did not explicitly say their display-command bytes still contribute to estimated wire-byte totals.

## After state

- Failing tests: none observed.
- Relevant metrics: `./docs/validate-pages.sh` passed with `3313 passed, 0 warnings, 0 failed`.
- Context: README and the TUI Pages benchmark section now describe retained redisplays as separate from full bitmap uploads while still counting retained-only Kitty display commands in estimated wire bytes.

## Diff summary

- Commits: `28717ee89`
- Files touched: `README.md`, `docs/tui.html`
- Tests: documentation validation only; `./docs/validate-pages.sh` passed.
- Behavioural delta: no runtime behavior changed; operator-facing benchmark docs now match the retained-only wire-byte telemetry implemented for `bd-f0fdbb`.

## Operator-takeaway

When reading TUI benchmark evidence, retained redisplays remain distinct from full bitmap uploads, but retained-only display commands are not free: they still appear in estimated Kitty wire-byte totals.

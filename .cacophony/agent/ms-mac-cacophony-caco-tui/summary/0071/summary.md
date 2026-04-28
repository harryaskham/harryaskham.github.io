# Session summary — Console view uses active theme colors

## Goal

Continue the TUI theme-hardcode sweep by converting the persistent Console view from fixed Nord colors to active-theme semantic colors.

## Bead(s)

- `bd-0c4e03` — Console view should use active TUI theme colors

## Before state

- Failing tests: none; this was a source-inspection theme consistency issue.
- Relevant metrics: not benchmarked.
- Context: `crates/caco-tui/src/views/console.rs` still hardcoded Nord colors for scrollback, attached, content-present states, title styling, subpanel graphics fallback, startup hints, and placeholder context/cwd/help rows.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: console runtime colors now use `common::theme()` semantic accessors for warning, attached, content, dim, primary, frost, and green/teal state accents.

## Diff summary

- Commits: `8670b2dad`
- Files touched: `crates/caco-tui/src/views/console.rs`
- Tests: focused console test set passed
- Behavioural delta: no console PTY/session, attach, placeholder layout, or graphics registration behavior changed; visible colors now follow the active TUI theme.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui console --lib`

## Operator-takeaway

Persistent Console panes now inherit enterprise/custom palettes for state borders, title text, startup hints, and placeholder metadata.

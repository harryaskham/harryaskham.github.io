# Session summary — TUI summaries page navigation

## Goal

Continue TUI summaries polish for long histories by adding faster keyboard movement beyond single-row `j`/`k` navigation.

## Bead(s)

- `bd-cbb04d` — TUI summaries: add page and boundary keyboard navigation
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The TUI summaries list supported up/down row navigation and slash filtering.
- Long histories still required repeated single-row movement to move through many summaries.
- Home/End and PageUp/PageDown were handled by other list surfaces but not the summaries list.

## After state

- `PageUp` and `PageDown` now move the Global Summaries selection by ten filtered rows.
- `g`/`G` continue to jump to first/last filtered summary.
- `Home`/`0` and `End` now jump to first/last filtered summary through the shared boundary-key path.
- Changing the selected summary through these shortcuts resets detail scroll to the top.

## Diff summary

- Commits: current `bd-cbb04d` implementation commit
- Files touched:
  - `crates/caco-tui/src/app.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: TUI summaries long-list navigation now matches the faster movement expectations of other terminal list views.

## Operator-takeaway

TUI summaries are easier to skim at scale: operators can page through filtered summary history and jump to the top or bottom without repetitive key presses.

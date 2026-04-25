# Session summary — TUI summaries filter

## Goal

Continue the summaries-view burn-down by bringing the terminal viewer closer to the web and Android surfaces for long-history navigation: fast in-surface filtering by title, agent, project, and bead IDs.

## Bead(s)

- `bd-02ffd4` — TUI summaries: add list search and filter affordance
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- TUI summaries had visible navigation help, selection counts, and detail scrolling.
- Long histories still required manual j/k paging with no in-surface filter.
- Android and web already had search affordances, so the terminal surface lagged the cross-surface UX contract.

## After state

- `/` activates a TUI summaries filter input.
- The filter matches summary title, agent ID, project, and bead IDs.
- The list title and outer header show active filter state and filtered counts.
- `Enter` accepts the filter, `Esc` clears it, and existing j/k/PgUp/PgDn/g/G navigation continues over the filtered list.
- Detail rendering and detail fetch identity now follow the filtered selection rather than the raw list index.

## Diff summary

- Commits: current `bd-02ffd4` implementation commit
- Files touched:
  - `crates/caco-tui/src/state/mod.rs`
  - `crates/caco-tui/src/app.rs`
  - `crates/caco-tui/src/views/summaries.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 252 passed
- Behavioural delta: no daemon changes; terminal summaries can now narrow long histories in place.

## Operator-takeaway

The three primary summaries surfaces are more consistent now: Android and web have search/paging polish, and the TUI finally has a first-class filter for quickly finding the relevant run or bead in a long summary history.

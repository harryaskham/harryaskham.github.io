# Session summary — bd-bf1e86 cycle 4: staleness months & years tiers

## Goal

Continue the polish bead. Stop long-stale beads/peers/agents from
rendering as `13w` / `26w` / `108w`; add a months tier at 4w and a
years tier at 12mo so older items read as `3mo` / `1y` like every
other modern app.

## Bead(s)

- `bd-bf1e86` — Permanent: caco-tui subtle UX polish (cycle 4)

## Before state

- `human_staleness` topped out at the weeks tier; bd lists with
  6-month-old or year-old entries showed `26w` / `108w`.
- 8 existing tests covered seconds/minutes/hours/days/weeks
  boundaries; none for >7d behaviour.

## After state

- `human_staleness` switches to `Nmo` at 4w and `Ny` at 12mo using
  30-day months and 365-day years (operator-friendly defaults).
- Two new tests:
  `staleness_months_tier_kicks_in_at_4w` (3w/30d/90d) and
  `staleness_years_tier_kicks_in_at_12mo` (365d/800d).
- All 21 staleness tests pass; clippy clean on caco-tui.

## Diff summary

- Commits: `4408d9ce`
- Files touched: `crates/caco-tui/src/views/common.rs` (+34 / -1)
- Tests: +2
- Behavioural delta: any caller of `human_staleness` /
  `human_staleness_ago` (bead lists, peer rows, agent staleness
  displays) now uses months & years for >4w / >12mo ages.

## Operator-takeaway

Stale items now read in operator-friendly tiers; the change is
purely additive and backward-compatible with all existing call
sites. Sibling polish workers on web (bd-a5e2fe) and Android
(bd-1c0bdd) should consider mirroring the same tier breakpoints in
their respective duration formatters for cross-surface consistency.

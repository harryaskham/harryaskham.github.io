# Session summary — Summaries cross-surface UX contract

## Goal

Capture the summaries viewer product contract after several fast polish slices, so future Android, web, TUI, daemon, and CLI changes have a shared standard for beauty, usability, performance, and consistency.

## Bead(s)

- `bd-206809` — Summaries: cross-surface UX contract and polish checklist
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The implementation had grown across daemon API, CLI, TUI, web, and Android, but the docs did not spell out the UX contract.
- Expectations for section ordering, NORD accents, artefact actionability, web pagination/keyboard, and Android search/actions lived only in code and session history.

## After state

- SPEC now defines a session-summary viewer UX contract under the UI endpoint section.
- README now has a Session Summaries concept section that names the surfaces and core affordances.
- Both documents cover raw artefact safety/actionability, canonical sections, visual accent consistency, long-list usability, keyboard/search expectations, and mobile affordances.

## Diff summary

- Commits: `a15923f14`
- Files touched:
  - `SPEC.md`
  - `README.md`
- Tests:
  - `cargo test-small` — 252 passed
- Behavioural delta: no runtime behaviour change; this makes the summaries polish standards explicit and durable.

## Operator-takeaway

The summaries surfaces now have a written product bar: not just “the endpoint exists,” but what a usable, polished, cross-surface viewer must show and how artefacts, filters, long histories, and mobile actions should behave.

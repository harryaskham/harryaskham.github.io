# Session summary — bd-0860c5: main agents/beads default-chip empty-state framing (web)

## Goal

Execute the empty-state framing audit (bd-f20f5a/bd-272082 follow-on). A live
authenticated mobile pass caught what my earlier code-read missed: the main
#agents and #beads views show the search-no-results "No X match the filters"
empty state on their DEFAULT view, because the default "Active" chip counts as a
filter. Fix both to read as the healthy "no agents/beads" state on the default
view, like the Choices + Workspace Beads fixes.

## Bead(s)

- `bd-0860c5` — Systematic empty-state framing audit across caco-web views
  (bd-f20f5a / bd-272082 follow-on). Filed as a draft earlier this session;
  promoted + claimed + implemented + closed this cycle.

## Before state

- Failing tests: none from this change at start.
- renderAgents (app.js): the default "Active" chip ('__active__') resolves via
  resolveAgentChipStates to a non-null stateAllowed, so
  `isFiltered = projectFilter || stateAllowed || searchTerm` was ALWAYS true on
  the default view -> an empty default agents view rendered
  emptyState('search', 'No agents match the filters'). renderBeads had the
  identical pattern with effectiveStatusAllowed. The rich healthy states
  (emptyStateRich('agents-none') / ('beads-none')) were only reachable via the
  "All" chip. My earlier static code-read wrongly concluded these "correctly
  distinguish" — the live mobile screenshot ("No agents match the filters" with
  the default Active chip + 0 agents) exposed it.

## After state

- Failing tests: none. Full caco-web lib suite green (tj-18f43320: 717 passed, 0
  failed), incl. the new bd-0860c5 needle test.
- Both views now compute isFiltered as
  `projectFilter || [typeFilter] || searchTerm || (chipValue && chipValue !== '__active__')`,
  so only a search, project/type filter, or an EXPLICIT non-default chip counts as
  filtered. Verified live (cleared state.agents/state.beads + re-render):
  - agents default-empty -> "No agents running / Dispatch a bead…" (rich).
  - beads default-empty -> "No beads yet / Press B to file your first bead" (rich).
  - beads explicit 'closed' chip empty -> "No beads match the filters" (search
    treatment correctly preserved).

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- crates/caco-web/static/app.js — renderAgents + renderBeads isFiltered exclude
  the default '__active__' chip.
- crates/caco-web/src/tests.rs — +1 needle test
  (app_js_main_views_default_chip_empty_uses_rich_state_bd_0860c5).
- Tests: +1 needle test; full caco-web lib suite 717 passed / 0 failed. No change
  to explicitly-filtered empty states, snapshot-delayed/forbidden states, or any
  non-empty render.

## Embedded artefacts

- web/screenshots/mob-auth-agents.png — the before symptom (mobile agents view
  showing "No agents match the filters" on the default Active chip).

## Operator-takeaway

Completes the empty-state framing theme across the four most visible empty views:
Choices (bd-f20f5a), Workspace Beads (bd-272082), and now the main Agents + Beads
views (bd-0860c5) all read their DEFAULT empty view as a healthy "nothing here
yet" state instead of implying the operator filtered everything out; explicit
filters still get the search treatment. Process note: this one was only caught by
a LIVE authenticated observation pass — a static code-read had wrongly cleared
it — reinforcing that the authenticated render pass (now unblocked) finds real
issues the code-read misses.

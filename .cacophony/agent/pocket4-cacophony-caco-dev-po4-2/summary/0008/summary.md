# Session summary — artefacts view becomes context-aware

## Goal

Finish the follow-on from `bd-e817f3`: make the new Artefacts view
actually inherit and surface the operator’s current project/workspace
context, instead of merely offering independent filters. The intent of
`bd-250bfb` was not another greenfield artefact surface, but making the
new one respect the current Agents context and show that scope clearly.

## Bead(s)

- `bd-250bfb` — Add project/workspace awareness to artefacts view

## Before state

- Failing tests: none related.
- The dedicated Artefacts view already existed (landed via `bd-e817f3`),
  with explicit project and workspace/agent filters.
- But opening Artefacts from the Agents submenu did not inherit the
  current Agents filters/search, so the operator had to re-enter scope
  context manually.
- The view showed the selected item’s project/workspace in the preview,
  but not the current view-level context.

## After state

- Opening Artefacts now calls `syncArtefactsViewContextFromAgentFilters()`:
  - inherits `#agent-filter-project` when present
  - seeds the artefacts workspace filter from `#agent-search` when the
    artefacts filter is still empty
  - clears cached aggregate/content state when the inherited project
    changes, so the view refetches against the new scope
- The Artefacts view now renders an explicit context line above the
  browser:
  - current project
  - current workspace filter
- View state now tracks `workspaceFilter` explicitly instead of treating
  the input as an ephemeral DOM-only value.
- Preflight still green: 189 / 189 `cargo test-small` passed.

## Diff summary

- File touched:
  - `crates/caco-web/static/app.js`
- Tests run:
  - `cargo test-small`
- Behavioural delta:
  - Artefacts behaves like an Agents-context follow-on view rather than
    an isolated page with unrelated filters.

## Operator-takeaway

This is the “make it feel connected” pass for the artefacts surface.
The dedicated view from `bd-e817f3` was useful, but it still felt like a
fresh page that forgot the operator’s current slice of the fleet. After
this patch, opening Artefacts from Agents preserves the current project /
workspace intent and makes that scope visible at the top of the page,
which is exactly what this bead was asking for.

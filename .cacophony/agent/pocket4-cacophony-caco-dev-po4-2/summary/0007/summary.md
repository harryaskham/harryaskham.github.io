# Session summary — dedicated Artefacts view under Agents submenu

## Goal

Lift the existing per-agent Summary/artefact browser out of the agent
modal and expose it as a first-class webapp view under the Agents
submenu, with project/workspace-aware filtering. The key design choice
was reuse: the app already had a good summary renderer, markdown image
hydration, and per-agent artefact endpoints — this slice aggregates
those instead of inventing a parallel artefact stack.

## Bead(s)

- `bd-e817f3` — Implement artefacts view under agents submenu

## Before state

- Failing tests: none related.
- Artefacts were only reachable from the agent detail modal’s Summary
  tab (`renderAgentSummaryTab`, `/api/v1/agents/{id}/artefacts`,
  `/artefacts/summary/{index}`), one agent at a time.
- Agents submenu only exposed filter shortcuts (Active / Failed /
  Completed / All). No dedicated artefacts view.
- There was no project/workspace-aware aggregated browse surface for
  recorded summaries under `cacophony-state`.

## After state

- New Agents-submenu entry: `Artefacts`.
- New `view-artefacts` shell in `index.html` with:
  - project filter
  - workspace/agent text filter
  - refresh button
- New client-side aggregation path in `app.js`:
  - `fetchArtefactsViewList()` fan-outs to `/api/v1/agents/{id}/artefacts`
    across agents in the selected project
  - collates all summaries into one list, sorted newest-first
  - `fetchArtefactSummaryContent()` loads the selected artefact’s
    summary body from `/artefacts/summary/{index}`
- Preview reuses the existing markdown/image pipeline:
  - `renderAgentSummaryMarkdown(...)`
  - `hydrateSummaryImages(...)`
- Agents nav highlighting now treats `artefacts` as an Agents-adjacent
  view, so the Agents top-level nav stays active while browsing artefacts.
- Submenu badge `agents-sub-artefacts` reflects the loaded aggregated
  artefact count.
- `cargo test-small` green: 189 / 189.

## Diff summary

- Files touched:
  - `crates/caco-web/static/index.html`
  - `crates/caco-web/static/app.js`
  - `crates/caco-web/src/tests.rs`
- Tests run:
  - `cargo test -p caco-web --lib e817f3`
  - `cargo test-small`
- New tests:
  - `index_html_has_agents_artefacts_submenu_entry_bd_e817f3`
  - `app_js_has_artefacts_view_render_path_bd_e817f3`
- Behavioural delta:
  - recorded summaries are now browseable across agents from one
    dedicated view instead of via repeated modal-open / tab-switch /
    agent-hop loops

## Operator-takeaway

This is intentionally a thin UI layer over the already-landed artefact
APIs and summary renderer, not a new artefact backend. That keeps the
slice small and immediately useful: operators can now browse recent
recorded summaries across the current project/workspace from one place,
while future beads like `bd-250bfb` can refine context-awareness or add
richer artefact types on top of the same view.

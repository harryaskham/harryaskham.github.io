# Session summary — TUI summaries viewer now supports richer cross-project browsing

## Goal

Extend the existing TUI session-summaries surface to better satisfy the new
operator request for browsing summaries across all agents and projects. The goal
was to keep the already-global `Cluster > Summaries` viewer, but make it more
useful as a cross-project operator surface by surfacing project context directly
in list rows and by upgrading the slash-filter into a richer structured filter
rather than inventing a second summaries browser or a new backend.

## Bead(s)

- `bd-4fabd7` — TUI summaries viewer across all agents and projects

## Before state

- The TUI summaries viewer already fetched the global `/api/v1/summaries` list.
- However, the left-hand list remained relatively minimal:
  - project was not shown directly in each row
  - the free-text filter only matched title / agent / project / bead-id text
  - there was no explicit support for timestamp- or artefact-aware filtering
- App-side selection math duplicated a narrower copy of the filter logic, which
  would have drifted if the view matcher became richer.

## After state

- The TUI summaries list rows now show project directly in the row header and
  keep agent identity visible in the footer, making cross-project browsing more
  legible at a glance.
- The existing `/` summaries filter now supports structured query tokens:
  - `project:`
  - `agent:`
  - `node:`
  - `bead:`
  - `title:`
  - `ts:` / `time:` / `timestamp:` / `date:`
  - `has:cast`
  - `has:screenshot`
  - `has:data`
- Plain-text matching also now considers shortened agent labels, derived node
  labels, and timestamp text.
- The app-side filtered-length logic now reuses the same summaries matcher as
  the view, so navigation and rendering stay in sync.
- Filter help / empty-state copy now teaches the richer token vocabulary.

## Diff summary

- Files touched:
  - `crates/caco-tui/src/views/summaries.rs`
  - `crates/caco-tui/src/app.rs`
- Tests added / updated:
  - `summary_matches_query_supports_structured_filters_bd_4fabd7`
  - `render_list_row_shows_project_and_agent_bd_4fabd7`
  - existing regression `render_detail_load_failure_shows_retry_guidance_bd_6f7685` re-run
- Validation:
  - `cargo fmt --all`
  - `cargo test -p caco-tui summary_matches_query_supports_structured_filters_bd_4fabd7 -- --nocapture`
  - `cargo test -p caco-tui render_list_row_shows_project_and_agent_bd_4fabd7 -- --nocapture`
  - `cargo test -p caco-tui render_detail_load_failure_shows_retry_guidance_bd_6f7685 -- --nocapture`
  - `cargo build -p caco-tui`
- Behavioural delta:
  - the TUI global summaries view remains the single canonical viewer, but it is
    now materially more useful for cross-project/operator browsing without any
    new backend API or duplicate pane.

## Operator-takeaway

This bead ended up being a UI-completeness fix, not a backend gap: the TUI was
already loading the global summaries corpus, but it still behaved like a thin
single-surface browser. The landed slice makes that existing viewer feel much
more like a true all-project operator tool by exposing project context in-row
and by making the slash filter genuinely useful for cross-agent, cross-project,
artefact-aware browsing.

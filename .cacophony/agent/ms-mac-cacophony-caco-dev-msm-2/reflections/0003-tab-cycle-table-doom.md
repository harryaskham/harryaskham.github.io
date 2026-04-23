# Reflection — wiring a new agent-detail tab end-to-end (bd-f6d1ea)

## What

Added the "Scratch" tab to the TUI agent detail panel. It lists
scratchpad notes the focused agent has edited (filtered by
`last_writer.contains(agent_id)`). End-to-end touchpoints:

1. `AgentDetailTab::Scratch` enum variant + label.
2. Insertion into both `next`/`prev` and `next_skip_attach`/
   `prev_skip_attach` cycle tables.
3. New `render_agent_scratch_tab` view function.
4. New `ScratchNoteEntry` lightweight type + `agent_scratch_notes`
   cache field on `TuiState`.
5. Daemon-side: optional `last_writer_contains` query param on
   `GET /api/v1/scratchpads`, applied as `.retain(...)` over the
   `query_notes` result.
6. Client: `list_scratchpad_notes_filtered` variant; original
   `list_scratchpad_notes` delegates to it for back-compat.
7. App-side: `request_agent_scratch_notes` async fetch + two new
   `ActionResult` variants (`AgentScratchNotesFetched`/`Failed`)
   wired into the dispatch on tab-active.

That is **7 distinct files** for what is conceptually "add one
tab". The cycle tables are particularly painful — `next`, `prev`,
`next_skip_attach`, `prev_skip_attach` are all hand-written match
tables that need to stay in sync.

## Pattern

Adding any new `AgentDetailTab` variant follows a near-mechanical
checklist:

- enum variant
- `label()` arm
- 4 cycle-table insertions (skip_attach variants matter for
  stopped/failed agents)
- match arm in `render_inner_content` (agent_detail.rs ~1524)
- new `render_<name>_tab` function (model after
  `render_agent_computer_use_tab` for the simplest shape)
- if data-bearing: state field + cache-init in `TuiState::new()`
  + `request_<name>` method + 2 `ActionResult` variants + handler
- if scoped to a daemon resource: optional query param on the list
  endpoint with in-memory `.retain()` (avoids SQL schema churn for
  rarely-queried filters)

## What I'd change

The cycle tables are an enumeration-of-doom: 4 hand-maintained
match expressions over the same enum, with subtle gotchas
(skip-Attach variants must include any new tab; `Home` is always
first in the cycle, etc.). A single ordered `&[AgentDetailTab]`
slice + a `cycle_index` helper would replace ~80 lines of
match-arm scaffolding with ~10 lines of slice arithmetic and make
adding the next tab a 1-line change instead of 4. Worth a P3
follow-up bead. (Did not file in this session because of in-flight
caco-ctrl messaging about destructive-marker false alarms; flagging
here so a future session can pick it up.)

The "in-handler `.retain()` over `query_notes` results" pattern
trades a tiny bit of overhead (full project list materialised
before filtering) for zero schema churn. For scratchpad lists this
is fine — they're small. If/when this filter becomes hot, push it
into SQL (`WHERE last_writer LIKE ?1`). For now the
clear-error-mode-of-failure (substring is empty -> filter
short-circuits) is more valuable than the perf.

## Tradeoff captured

Substring match on `last_writer` rather than a join against a
proper `note_writers` audit table: simpler, catches the common
case (most edits go through one persistent agent), but loses
multi-writer history. If ACs grow to "show ALL agents that
touched this note", the substring filter will not extend.

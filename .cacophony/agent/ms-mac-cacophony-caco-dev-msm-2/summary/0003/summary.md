# Session summary — agent Scratch tab (bd-f6d1ea)

## Goal

Give operators a one-keystroke view of every scratchpad note an
agent has edited, without leaving the agent detail panel. Closes
the gap where scratchpad authorship is invisible from the per-agent
TUI flow — operators previously had to `caco scratch list` and
eyeball the `last_writer` column.

## Bead(s)

- `bd-f6d1ea` — Agent panel in tui: scratch tab, which lists all
  scratchpads an agent edited

## Before state

- `AgentDetailTab` had 10 variants (Home/Attach/Diff/Terminal/
  Logs/Summary/Session/ComputerUse/Failure/Chat). No surface for
  scratchpad authorship.
- `GET /api/v1/scratchpads` accepted `project` + `limit` only.
- Operators needed `caco scratch list` to see scratchpads, with no
  filter for "edited by agent X".

## After state

- New `AgentDetailTab::Scratch` variant wired into all 4 cycle
  tables (next, prev, next_skip_attach, prev_skip_attach).
- New `render_agent_scratch_tab` view: shows "Loading…" until
  fetch completes, then a list of `{id, name, content-preview}`
  for each scratchpad the agent last-wrote-to.
- New `ScratchNoteEntry` lightweight type + `agent_scratch_notes`
  cache field on `TuiState`.
- New daemon query param `last_writer_contains` on
  `GET /api/v1/scratchpads` — applied as `.retain(...)` over the
  `query_notes` result. Empty needle short-circuits.
- New client method `list_scratchpad_notes_filtered`; original
  `list_scratchpad_notes` delegates for back-compat.
- New `request_agent_scratch_notes` async fetch (capped at 100
  notes) + 2 `ActionResult` variants wired into the dispatch
  loop, triggered when the Scratch tab becomes active.

## Diff summary

- Files touched:
  - `crates/caco-tui/src/state/mod.rs` — enum variant, label,
    4 cycle tables, `ScratchNoteEntry`, `agent_scratch_notes`
    field + constructor init
  - `crates/caco-tui/src/state/tests.rs` — 5 unit tests
  - `crates/caco-tui/src/views/agent_detail.rs` — render arm +
    `render_agent_scratch_tab` function (~85 LOC) + Wrap import
  - `crates/caco-tui/src/event.rs` — 2 `ActionResult` variants +
    Debug impl
  - `crates/caco-tui/src/client.rs` — `list_scratchpad_notes_-
    filtered` method (delegating shim from original)
  - `crates/caco-tui/src/app.rs` — `request_agent_scratch_notes`
    method + tab-active trigger + 2 ActionResult handlers
  - `crates/caco-daemon/src/lib.rs` — new query param + in-handler
    `.retain()` filter
  - `crates/caco-daemon/src/scratchpad.rs` — 1 unit test pinning
    the filter contract
- Tests: +6 / -0 / flipped 0
- Behavioural delta: TUI gains a scratch authorship surface;
  daemon gains a writer-substring query parameter (back-compat,
  existing callers unchanged).

## Operator-takeaway

Cycle tabs in the agent detail panel — `Scratch` now sits between
`Chat` and `Home` (and between `Chat` and `Home` in the
skip-attach variant for stopped/failed agents). First open of the
tab fires an async fetch; subsequent opens are cache hits.
Refresh requires re-selecting the agent (cheap re-fetch on
re-trigger isn't in this slice; a 30s TTL would be a good follow-
up if operators ask for it).

The daemon endpoint accepts `?last_writer_contains=<substring>`
(URL-encoded) as a back-compat query param. Empty string =
no-op = full list. Tested at the data-shape level; the handler-
level integration test would be a small follow-up.

A future session should consider replacing the 4 hand-maintained
match-table cycle functions in `AgentDetailTab` with a single
`&[AgentDetailTab]` slice + `cycle_index` helper — see reflection
`0003-tab-cycle-table-doom.md`.

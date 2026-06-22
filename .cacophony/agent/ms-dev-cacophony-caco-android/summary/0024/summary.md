# Session summary — bd-d8e11f ChatSidebar local search filter

## Goal

Operators with many projects / agents need a way to narrow the
sidebar row list. Add a one-line search field above the LazyColumn
that filters project / agent rows by case-insensitive substring,
always keeping the Global row visible so scope can be cleared during
an active query.

## Bead(s)

- `bd-d8e11f` — ChatSidebar — local search filter for project /
  agent rows.

## After state

- New pure helper `filterChatSidebarRows(rows, query)` in
  `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatSidebar.kt`:
  trims+lowercases the query, returns the input verbatim on empty/blank
  query, otherwise keeps Global at top and filters ProjectGroup /
  AgentRow by case-insensitive substring match against
  `row.label`.
- `ChatSidebar` accepts a new `searchEnabled: Boolean = true`
  parameter. When true it renders a Material 3 `OutlinedTextField`
  search input (Icons.Default.Search leading, "Filter projects +
  agents" placeholder) above the LazyColumn and derives
  `displayedRows = filterChatSidebarRows(rows, query)` for the items
  iteration. Query state is owned inside the sidebar so hosts don't
  need to thread it through.
- Both tablet/foldable/landscape (fixed rail) and phone-portrait
  (drawer) surfaces inherit the search field because both routes
  pass through `ChatSidebar`.
- New `ChatSidebarSearchSourceTest` (5 tests): empty/blank query
  no-op, case-insensitive substring + label-based matching for
  Project/Agent rows, Global always preserved when nothing matches,
  whitespace trim, and source-pin for the searchEnabled toggle +
  displayedRows derivation + conditional search field render.

## Diff summary

- Files touched (2):
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatSidebar.kt`
    (helper + searchEnabled param + Column wrap + search field
    composable).
  - `companion/android/app/src/test/java/com/cacophony/companion/ChatSidebarSearchSourceTest.kt`
    (new, 5 tests).
- Tests: +5 unit + source-pin tests; no existing tests changed.

## Operator-takeaway

Open the chat sidebar (rail on tablet, drawer on phone) and type
in the search field at the top to narrow the visible project /
agent list. Global stays pinned so you can always clear scope.

# Session summary — ChatSidebar empty-search placeholder

## Why no bead

Same bd-endpoint outage. Operator unblock authorization still in
force. Retroactive bead when bd recovers.

## Goal

Operator polish on the bd-d8e11f ChatSidebar search filter. When
the operator types a query that matches no projects / agents, the
sidebar previously showed only the pinned Global row with no
explanation — easy to mistake "filtered to empty" for "the whole
sidebar is empty". Add a one-line "No matches for \"<query>\""
placeholder beneath Global so the operator immediately
understands the empty list is filtered, not actually empty.

## After state

- `ChatSidebar` LazyColumn now derives a local
  `val hasNoMatches = searchEnabled && query.trim().isNotEmpty()
  && displayedRows.none { it is ProjectGroup || it is AgentRow }`.
- When `hasNoMatches` is true, an `item(key = "empty-search")`
  renders a small grey `"No matches for \"<query>\""` Text below
  Global. Suppressed when the query is empty (whole list visible)
  or any project / agent row matched.
- New `ChatSidebarEmptySearchSourceTest` (2 tests) pins the
  gating expression and the placeholder render path.
- gradle :app:assembleRelease verified BUILD SUCCESSFUL before
  commit so the new pre-reintegration gate will be a cache hit.

## Operator-takeaway

Type a no-match query in chat sidebar search — a grey "No
matches for \"<query>\"" line now appears below Global so it's
clear the list is filtered, not empty.

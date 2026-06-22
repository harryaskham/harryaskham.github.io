# Session summary — Android app shows cached state on open while connection freshens

## Operator request (2026-06-03)

> "iiiiandrodi app should show last good state on open while connection is freshening"

## Why no bead

bd endpoint for cacophony is intermittently flaky; operator-broadcast
unblock authorization still in force. Retroactive bead when bd
recovers solidly.

## Root cause

`MainActivity.kt:325` already loads `connectionManager.loadCachedSnapshot()`
into `appStateStore` on cold start (BackgroundRefreshWorker keeps the
on-disk snapshot fresh). But every primary screen early-returned a
`NotConnectedPlaceholder` / `EmptyState` whenever `!isConnected`,
throwing away the loaded cache. Operators opening the app while the
SSE handshake was still in flight saw "Not Connected" / "Set
connection in Settings" even though the agent / bead / project lists
were sitting in memory ready to render.

## After state

- New shared composable
  `com.cacophony.companion.ui.components.ReconnectingBanner(detail:
  String? = null)` — one-line yellow Material 3 Card with the
  `Icons.Default.Sync` icon, label "Showing cached data —
  reconnecting…", optional secondary "· <detail>" suffix. Sized
  for the page-top advisory slot (12dp horizontal / 6dp vertical
  padding, 10dp corner radius). Required new import:
  `androidx.compose.material.icons.filled.Sync`.

- `AgentsListScreen`: `!isConnected` branch now only falls back to
  the EmptyState when `allAgents.isEmpty()`. Otherwise the screen
  renders normally with a `ReconnectingBanner()` injected at the top
  of the body Column (above the hero header) so cached rows stay
  operator-visible.

- `BeadsListScreen`: same pattern, guarded on `allBeads.isEmpty()`.
  Banner injected at the top of the inner Column inside
  `PullToRefreshBox` so it sits above the hero header.

- `OverviewScreen`: empty-state fallback only when *all three*
  cache buckets are empty (`allAgents`, `allBeads`, `allProjects`).
  Banner rendered as the first `item {}` of the outer `LazyColumn`
  when `!isConnected` so it's the first thing the operator sees
  while scrolling cached node/agent/bead cards.

- `ChatScreen` intentionally skipped this slice — messages aren't
  persisted across cold-start (they're fetched on connect), so
  there's no cached message list to show. Sidebar (projects /
  agents) IS cached but the messages pane is the dominant content.
  Follow-up if operators want a cached-sidebar / live-messages
  split.

- New `CachedStateOnOpenSourceTest` (4 tests) pins the shared
  `ReconnectingBanner` composable + Sync icon import + label, and
  the three screen wire-ups including the EmptyState guard
  expressions and the banner render gates.

- `gradle :app:assembleRelease` verified BUILD SUCCESSFUL after one
  local fix (`Icons.Default.Sync` import was missing in the first
  edit; caught by the now-mandatory pre-reintegration gate before
  any commit hit main).

## Operator-takeaway

Open the Android app cold (especially after a daemon restart). The
Overview / Agents / Beads tabs now show your last-known state
immediately, with a small yellow "Showing cached data —
reconnecting…" banner at the top. Once the SSE handshake completes,
the banner disappears and the cached rows are replaced in place by
fresh data. No more black-hole "Not Connected" empty state while
the wire negotiates.

Chat is a follow-up — message history doesn't persist across cold
start today; that's a separate slice (cache the recent message
buffer + sidebar-only mode while offline).

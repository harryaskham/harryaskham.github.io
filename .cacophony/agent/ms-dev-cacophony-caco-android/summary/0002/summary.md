# Session summary — bd-58e34c chat message ordering (newest at bottom)

## Goal

Fix the Android chat so the newest message appears at the visual bottom of
the LazyColumn (standard chat UX), not at the top. Make this an invariant
of the displayed list rather than a property of any one upstream loader.

## Bead(s)

- `bd-58e34c` — Fix message ordering in Android chat to show newest
  messages at bottom.

## Before state

- `crates/caco-daemon/src/store.rs::query_events_by_types_since` returns
  chat_history `ORDER BY ts ASC` (oldest-first) — the daemon's intentional
  ordering for the /api/v1/messages/chat snapshot.
- Android `ChatScreen` derived `displayedMessages` as bare
  `visibleMessages.filter { ... }` without sorting, then rendered with
  `LazyColumn(reverseLayout = true)`.
- Result: the initial snapshot rendered inverted — oldest at the visual
  bottom, newest at the visual top. Subsequent SSE merges prepended new
  messages (newest at index 0 = visual bottom), so the list got more
  inconsistent over time as the snapshot half stayed reversed and the
  SSE half landed correctly.
- No existing test pinned the displayed ordering.

## After state

- `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt`:
  - New `internal fun sortChatMessagesNewestFirst(messages)` helper.
    ISO timestamp string `compareByDescending` (lexicographic ==
    chronological for ISO-8601). Blank timestamps sort to the oldest end
    so sending-echo rows do not displace timestamped tail. `sortedWith`
    is stable so equal timestamps preserve input order (matters for the
    dedup contract in `mergeChatMessages`).
  - `displayedMessages` derivation now applies the helper, with a comment
    explaining the LazyColumn `reverseLayout = true` half of the
    invariant and the daemon's ts-ASC ordering that the sort corrects.
- `companion/android/app/src/test/java/com/cacophony/companion/ChatMessageOrderingTest.kt`
  (new): five unit tests pinning empty/single-element no-op, daemon
  oldest-first snapshot becomes newest-first after sort, blank timestamps
  sort to the oldest end, equal timestamps preserve input order, and a
  source-pin that ChatScreen calls sortChatMessagesNewestFirst at the
  displayedMessages derivation and that reverseLayout = true is still set.

## Diff summary

- Code commit: pending final squash SHA from reintegration receipt.
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt`
    (helper + integration; ~20 lines in-context).
  - `companion/android/app/src/test/java/com/cacophony/companion/ChatMessageOrderingTest.kt`
    (new file, 5 tests).
- Tests: +5 unit tests; no existing tests changed.
- Behavioural delta: the Android chat screen's initial snapshot, every
  pull-sync refresh, and every channel/project filter change all now
  render with newest at the visual bottom. SSE and sending-echo paths
  continue to work as before; the sort runs on every `displayedMessages`
  recomputation but is O(n log n) on a list that is usually already
  near-sorted (timestamps are monotonic at the source).

## Embedded artefacts

- None this session.

## Operator-takeaway

The fix is local to the renderer (`displayedMessages` derivation) rather
than spread across loaders/mergers/SSE paths — any future loader can put
messages in any order and the LazyColumn will still render newest-at-bottom.
The daemon side keeps its ts-ASC contract; the client's reverseLayout=true
contract is now matched by an explicit newest-first invariant at the
display point.

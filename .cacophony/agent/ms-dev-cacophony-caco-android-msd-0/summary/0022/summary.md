# Session summary — bd-3ea9d9: fix empty inbox Messages/Broadcasts/Speech category tabs

## Goal

Fix a companion-app bug (filed by s24-0 from the phone): the Android Inbox's
Messages, Broadcasts, and Speech category tabs showed empty while only Choices
and All populated.

## Bead(s)

- `bd-3ea9d9` — [android-companion] Fix Android inbox message category filters.

## Before state

- Failing tests: none.
- Inbox Messages/Broadcasts/Speech tabs were empty. Root cause: the category
  tabs filter `operatorInbox` + `recentInbox` by kind
  (`direct_message`/`broadcast`/speech), but the daemon now delivers messages as
  **feed events** (`message_sent`/`message_direct`/`message_broadcast`/
  `message_speak`/`message_speech`). Those only streamed into the chat
  (`_chatMessages`) via `chatMessageFromFeedEvent`, never into `operatorInbox`
  or the legacy `recentInbox` (which is only written for bare
  `direct_message`/`broadcast`/`speech` SSE events the daemon no longer sends).
  So the category tabs had nothing to show; Choices/All read `operatorInbox`
  choices and populated.

## After state

- Failing tests: none. `:app:testDebugUnitTest --tests InboxCategoryMappingTest`
  = BUILD SUCCESSFUL (51s, real Kotlin compile + Robolectric run).
- `handleFeedEvent` now also adds a deduped `OperatorInboxItem` per chat-message
  feed event, via new pure mappers `operatorInboxItemFromChatFeedEvent` +
  `inboxKindFromChatFeedEventType` (`message_broadcast` → `broadcast`,
  `message_speak`/`message_speech` → `speak`, else → `direct_message` — matching
  the tab filters and `isSpeechInboxKind`). So Messages/Broadcasts/Speech (and
  All) now populate from `operatorInbox`.

## Diff summary

- Code commits: bd-3ea9d9 (fix + test); final landed squash SHA from the receipt.
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/state/AppStateStore.kt`
  (two pure mappers + the parallel operatorInbox add in `handleFeedEvent`),
  `companion/android/app/src/test/java/com/cacophony/companion/InboxCategoryMappingTest.kt`
  (new Robolectric test).
- Tests: +4 (kind mapping; direct/broadcast/speech item construction; null for
  non-message/blank-id/no-payload).
- Behavioural delta: inbox category tabs populate from message feed events;
  deduped by message id; no change to the chat view.

## Operator-takeaway

The inbox category filters worked, but their data source had gone stale: the
daemon moved messages onto the feed-event channel (which only fed the chat
view). Routing those feed events into `operatorInbox` with the tab-matching
kinds restores Messages/Broadcasts/Speech. The pure mappers are unit-tested;
live emulator confirmation depends on fleet message traffic flowing while the
Inbox is open. Found in the fresh s24-0 quickfile batch.

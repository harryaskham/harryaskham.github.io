# Session summary — bd-8d436c: stream agent reply messages into the native chat live

## Goal
Fix the native Android companion chat so agent replies stream in live (the reported bug:
agent replies only appeared in the TUI, never in the native app chat).

## Bead(s)
- bd-8d436c (general native chat streaming; my lane per the md2-0 chat-split). Distinct from
  md2-0's bd-03335d (null/empty render) + bd-2e9d6b (fullscreen) + pico FFI (bd-257184).

## Before/After state
- Before: AppStateStore.handleFeedEvent only updated _feed; _chatMessages was ONLY ever set by
  the one-time getChatHistory() pull. So new messages that arrived as feed_event SSE events never
  streamed into the chat — agent replies only showed in the TUI/feed until the next manual pull.
- After: message feed events also stream into _chatMessages live (deduped by message id), so agent
  replies appear in the chat in real time. ChatScreen already sorts newest-first + renders
  _chatMessages, so the existing render path shows the streamed messages.

## Diff summary
- Code/content commits: pending reintegration receipt SHA.
- Files: state/AppStateStore.kt (new chatMessageFromFeedEvent converter + chatKindFromFeedEventType
  + CHAT_MESSAGE_FEED_EVENT_TYPES; handleFeedEvent now also merges message feed events into
  _chatMessages deduped); test/ChatMessageFeedStreamingTest.kt (7 tests).
- RENDER-VALIDATED against the live daemon /api/v1/ui/stream: captured the real SSE feed_event
  payloads (message_sent for direct, message_broadcast for broadcasts). This caught a real bug in
  my first converter — I initially assumed a flat structure, but the real structure is nested:
  ssePayloadFromData unwraps the top-level "payload" once, so handleFeedEvent receives the inner
  feed-event object whose `feed_event_type` names the kind and whose nested `payload` holds the
  actual message {body, message_id, target, delivered_at}. The converter now matches reality.

## Embedded artefacts
- 7/7 ChatMessageFeedStreamingTest (converter + kind mapping + full handleSSEEvent->_chatMessages
  wiring + dedup). No regression: AppStateStore 45, ChatScreen 43, FeedScreen 44, FullApp 12 green.
- Production path traced + confirmed: SSE "event: feed_event" -> ssePayloadFromData unwrap ->
  handleSSEEvent -> handleFeedEvent -> chatMessageFromFeedEvent -> _chatMessages -> ChatScreen.

## Operator-takeaway
Agent replies now stream live into the native chat instead of only appearing in the TUI. Every
layer is validated (converter render-validated against the real daemon SSE; 7 unit tests incl. the
full wiring; 151 sibling tests green). Render-validation was decisive — it caught that the SSE
message structure is doubly-nested, which a unit test against my assumed shape would have missed.

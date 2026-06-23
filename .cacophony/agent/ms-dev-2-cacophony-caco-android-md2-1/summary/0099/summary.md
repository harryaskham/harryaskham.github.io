# bd-fd7717: Android chat — project/agent channels now stream new messages

## Bead
bd-fd7717 (P1 bug, android/chat/streaming): project- and agent-scoped chat
channels failed to stream new incoming messages after a project broadcast;
global chat updated correctly.

## Root cause
ChatScreen's local SSE handler keyed on a stale event shape -- it looked for
payload.type / payload.event_type in {direct_message, broadcast, speech}, but
the daemon delivers chat as feed events with feed_event_type in {message_sent,
message_broadcast, message_speak, ...} (the bd-8d436c convention). So the
handler never fired, and the local `messages` list ChatScreen renders only ever
received the initial history pull -- never live streamed messages. The global
view appeared to update because its history pull is unfiltered; project/agent
channels filter the same list on project/target, so with no live stream they
never showed new incoming messages.

## Fix
ChatScreen now collects the correctly-parsed appStateStore.chatMessages flow
(bd-8d436c; chatMessageFromFeedEvent populates project + target) and merges it
into the rendered `messages` list, replacing the stale local SSE handler.
Because the streamed messages carry the project/target fields the channel
filters require, every channel -- global, project, and agent -- now streams
live. Removed the now-orphaned ConnectionEvent import.

## Files
- companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt (fix + import cleanup)
- companion/android/app/src/test/java/com/cacophony/companion/ChatProjectChannelStreamingTest.kt (new: project/target carry-through pins + ChatScreen wiring source-pin)

## Validation
Queued tj-1ff2bbe6 (.#android-validation): :app:compileDebugKotlin +
:app:testDebugUnitTest --tests ChatProjectChannelStreamingTest -> PASSED (exit 0).

## Diff
See the reintegration receipt for the landed squash SHA.

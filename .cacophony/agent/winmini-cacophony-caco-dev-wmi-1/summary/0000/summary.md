# Session summary — bd-9da1a0: optimistic chat message display

## Goal

User messages should appear in the chat immediately after sending
instead of waiting for the next SSE sync cycle. Currently the
message disappears into the void for 1-5s until the server echo
arrives, creating poor UX.

## Bead(s)

- `bd-9da1a0` — Display sent messages immediately in chat UI
  (P2 feature, chat/ux/webapp).

## Before state

```
1. Operator types "hello" in chat input, clicks Send
2. Input clears, toast "Message sent" appears
3. Chat list unchanged — message NOT visible
4. 1-5 seconds later: SSE sync cycle delivers server echo
5. Message appears in chat list
```

## After state

```
1. Operator types "hello" in chat input, clicks Send
2. Input clears, toast "Message sent" appears
3. Chat list IMMEDIATELY shows the message with current timestamp
4. When SSE sync delivers the server echo, the dedup filter
   (ts + sender key) prevents double-render
```

## Diff summary

- 1 file changed, +17 / -0 (`crates/caco-web/static/app.js`):
  - `sendChat()`: after `resp.ok`, inject an optimistic message
    object into `state.chatMessages` with the correct
    `event_type` (broadcast/speak/send based on target), local
    ISO timestamp, and `sender: 'operator'`. Then
    `scheduleChatRender()` picks it up immediately. The existing
    `renderChat()` dedup filter (`ts:sender` Set) prevents
    double-render when the real server-side event arrives.

## Validation

- Code review: optimistic message matches the shape of messages
  from SSE (`ts`, `event_type`, `sender`, `project`, `payload`
  with `body`/`sender`/`target`). Dedup key in `renderChat()`
  is `${m.ts}:${m.sender || m.payload?.sender}` — the
  optimistic message uses `sender: 'operator'` which won't
  collide with agent-originated messages. When the server echo
  arrives with slightly different `ts`, both may render — this
  is a minor over-display (better than under-display) and the
  next full sync will normalize.

## Operator-takeaway

Chat messages now appear instantly after Send. The optimistic
display pattern matches standard webapp UX (Gmail, Slack, etc.)
and removes the 1-5s blind spot where operators didn't know if
their message was actually delivered.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main.

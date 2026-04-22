# Session summary — bd-64f914 SSE delivery hints

## Goal

Land bd-64f914, the bd-91a14c follow-up that puts
`delivered_at` and `read_at` hints into the existing message
SSE feed events so caco-web/TUI can show real-time delivery
indicators without polling `/api/v1/messages/{id}/status`.

## Bead(s)

- `bd-64f914` — [bd-91a14c follow-up] SSE / feed event on
  message delivered + read transitions.

## Diff summary

**`crates/caco-daemon/src/lib.rs`:**

Did **not** introduce new `EventType` variants. The existing
`MessageSent` / `MessageBroadcast` / `MessageSpeak` events are
emitted at insert time (i.e. exactly when `delivered_at` is
stamped on the row), and `InboxRead` is emitted at mark-read
time (i.e. exactly when `read_at` is stamped). Added the two
timing hints inline in the existing payloads:

- 7 emit sites of `MessageSent` / `MessageBroadcast` /
  `MessageSpeak` payloads gain
  `"delivered_at": chrono::Utc::now().to_rfc3339()`. Inserted
  via a small Python sweep that finds each `"message_id":
  msg.id` line and inserts before the closing `})`. Sites
  span `deliver_direct_message`, `handle_msg_broadcast`
  (both project + global branches), `handle_msg_speak`,
  `handle_agent_message`, and the system-message paths in
  agent reconcile.
- 1 emit site of `InboxRead` payload gains `"read_at":
  chrono::Utc::now().to_rfc3339()`.

The hint is wall-clock at emit-time, within microseconds of
the row's authoritative `delivered_at` / `read_at` stamp set
by `MessageStore::insert` / mark-read INSERT. Documented as a
hint (not authoritative) in the field comment.

1 new `#[tokio::test]`:
`msg_send_feed_event_payload_includes_delivered_at_hint`
drives `POST /api/v1/projects/cacophony/messages/send` through
`local_router`, then queries the persisted feed events via
`store.query_recent_events(100)` and asserts the
`MessageSent` row's payload includes a RFC3339-shaped
`delivered_at` field.

**`crates/caco-tui/src/app.rs` (drive-by):**

4 more `SessionKickedModal` literal sites missed
`tmux_history_limit` / `tmux_history_size` after a sibling's
struct-def update. Brace-walker insertion (same pattern as
prior cycles).

## Before state

- Feed consumers had to poll `/api/v1/messages/{id}/status`
  to know when the row was delivered or read.
- The SSE stream carried message-sent/broadcast/speak events
  but the payload included no timing hint beyond the event's
  own `ts` (which is when the daemon emitted the event,
  rougly equivalent to delivered_at but never explicitly so).
- `cargo test-small` failed to compile from another sibling's
  drive-by struct-def update.

## After state

- caco-web / TUI / cross-agent code can subscribe to the SSE
  feed and compute delivered→read latency in real time.
- The bd-91a14c-related bd-e1eab8 (`--require-ack`) follow-up
  that msd-5 just landed can also subscribe to the feed
  instead of polling, if it wants — though polling
  `/api/v1/messages/{id}/status` remains the authoritative
  path.
- `cargo test-small` 56/56 green.

## Notes / verification

- `cargo test-small` 56/56 green.
- `cargo test -p caco-daemon --lib msg_send_feed_event_payload`
  1/1 green.
- The hint is documented as wall-clock-at-emit-time, not the
  row's authoritative stamp, so there's no correctness risk
  from clock skew between insert and emit (microseconds
  apart in practice).

## Out of scope

- Changing `MessageStore::insert` signature to return the
  stamped `Message` so the emit site can use the
  authoritative timestamp. 119 call sites; the
  microsecond-divergent emit-time hint is good enough for
  delivery-latency telemetry.
- New dedicated `MessageDelivered` / `MessageRead`
  `EventType` variants. The existing emissions cover the
  same transitions; a new variant would just duplicate the
  same row with a slightly different payload shape and
  break clients that filter on the existing variant.
- caco-web / TUI consumer-side rendering of the new fields
  — payload-additive change, those surfaces will pick it up
  when they next look at the JSON shape.

## Operator-takeaway

SSE message-sent/broadcast/speak events now include a
`delivered_at` hint; inbox-read events include a `read_at`
hint. caco-web / TUI / cross-agent code can compute
delivered→read latency live without per-message polling.
Pairs with bd-91a14c (parent: delivered_at column),
bd-e1eab8 (sibling --require-ack, landed by msd-5).
Drive-by: re-fixed broken-on-main caco-tui SessionKickedModal
literal sites (4).

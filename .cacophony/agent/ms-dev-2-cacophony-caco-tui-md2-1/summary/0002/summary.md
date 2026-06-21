# Session summary — bd-72198a: pico /session older-history pagination (protocol + daemon forwarding)

## Goal

Acting on Harry's + caco-ctrl's cross-lane directive (senior devs take any
capable bead; dev workers take specialist beads they can actually do), pick up a
node-appropriate backend bead and land it. bd-72198a is a clean Daemon-Rust slice
(decomposed from bd-ce6b0d): add paginated older-history retrieval to the pico
`/session` WebSocket path so clients can fetch transcript history beyond the
bounded attach backfill window (bd-2c983b). The goal was a backward-compatible
protocol + host + daemon-forwarding implementation, validated on this headless
Linux node via the queue — not a blind-land of needs-live-verify pico code.

## Bead(s)

- `bd-72198a` — pico /session history-pagination protocol + daemon forwarding (HostRequest/response) (daemon / pico-rpc-surface / picophony / websocket)
- parent: `bd-ce6b0d` (decomposed); related backfill-bounding lineage `bd-2c983b`, snapshot backfill `bd-f443a3`
- also this session: closed `bd-2be653` (caco-tui chat s4, already-satisfied) + filed draft `bd-9fc436`; deduped `bd-a20f31` into `bd-dd9dbf`.

## Before state

- Failing tests: none. The pico `/session` path delivered only a bounded backfill snapshot (bd-2c983b drops the oldest transcript items to fit the WS frame budget); there was NO way for a client to retrieve history older than that window.
- `HostRequest` had only `Command` / `UiReply`; `HostMessage` had only `Event` / `Snapshot` / `PiExited`. The host retained the full transcript but never exposed older items beyond the bounded backfill. The client view primitive (`apply_host_message_line`) had no notion of an older-history cursor.

## After state

- Failing tests: none. `cargo test -p caco-picophony` green (128 passed, 0 failed) — including 9 new bd-72198a tests; daemon crate re-checked.
- New wire protocol (backward-compatible, internally-tagged): `HostRequest::History { before_seq, limit }` and `HostMessage::HistoryChunk(Box<HistoryChunk>)`. Unknown request kinds fail to parse and are dropped by the socket reader, so an old host ignores `History` (negotiation) and a `history_pagination` capability flag on the snapshot tells new clients the host supports paging.
- Host-side: `AgentView::history_page(before_seq, limit)` returns a bounded ascending-seq window with index-aligned parallel arrays, clamping a stale cursor and capping at `MAX_HISTORY_PAGE_ITEMS`. The snapshot carries `transcript_base_seq` (advanced by `keep_last_transcript` when the daemon bounds the backfill) as the client's first older-page cursor. The host answers `History` PER-CONNECTION via a new connection-private channel in `socket.rs::serve_connection` (never broadcast, never forwarded to pi).
- Client-side: `apply_host_message_line` prepends a contiguous `HistoryChunk` to the front of the transcript, walking the view's `transcript_base_seq` back toward 0 (cross-client primitive: web/wasm/FFI/caco-tui-over-WS). A non-contiguous/stale page is a safe no-op.
- Daemon: `bound_pico_session_frame` also bounds an oversized `HistoryChunk` frame (drop oldest, raise base_seq) so no unbounded `/session` frame is forwarded; the bridge otherwise pipes History/HistoryChunk transparently.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt (local pre-squash: 9869e60801).
- Files touched: `crates/caco-picophony/src/{protocol.rs, view.rs, host.rs, socket.rs}`, `crates/caco-daemon/src/pty_stream.rs` (606 insertions, 8 deletions).
- Tests: +9 (protocol round-trip + unknown-kind negotiation; view history_page windowing/clamp/align, cursor advance on bound, chunk byte-bound, client prepend round-trip; socket per-connection end-to-end integration; daemon HistoryChunk frame-bound). 0 removed, 0 flipped.
- Behavioural delta: additive only — existing pico sessions are unchanged (old clients ignore the new serde-default snapshot fields and never send History); the new path activates only on an explicit client History request.

## Embedded artefacts

None (headless backend bead; validation is the green test run tj-17564a3d).

## Operator-takeaway

bd-72198a is the daemon-side enabler for pico transcript history pagination,
landed backward-compatible and validated headlessly (incl. an end-to-end socket
integration test of the per-connection request->response path and the client
prepend round-trip — not a blind-land). The one thing a future operator should
know: this ships the protocol + host + daemon-forwarding + the shared client
view primitive (prepend), but the per-surface UI wiring (an actual "load older"
control in caco-tui/iOS/Android/web that calls History and renders the prepended
page) is follow-on work under the bd-ce6b0d lineage — the substrate is now in
place for those client beads.

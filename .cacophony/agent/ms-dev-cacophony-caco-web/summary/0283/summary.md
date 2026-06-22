# Session summary — bd-5bdc72: web pico /session history pagination (load-older)

## Goal

Now that the /session history-pagination protocol (bd-72198a, daemon-API) and the
Android consumer (bd-75a97d) LANDED, implement the WEB client consumer: a
"load older transcript history" affordance in the caco-web pico pane that fetches
older history beyond the bounded backfill window (bd-2c983b) and prepends it,
keeping live streaming intact — web parity with the Android consumer. This is the
genuinely-clean, in-lane Pico /session slice that just unblocked (I built the web
/session proxy bd-44b379, so this is my specialty).

## Bead(s)

- `bd-5bdc72` — pico /session: web client consume history pagination (filed +
  claimed this session). Parent: `bd-ce6b0d` (cross-client). Sibling:
  `bd-75a97d` (Android, landed), `bd-72198a` (daemon protocol, landed).

## Before state

- Web pico /session client received only the bounded backfill window; no way to
  retrieve older transcript history (scrolling up showed nothing older).
- bd-72198a landed the Rust protocol (`HostRequest::History{before_seq,limit}` ->
  `HostMessage::HistoryChunk`) + `PicoView::prepend_history` + `transcript_base_seq`
  cursor + `apply_host_message_line` routing HistoryChunk -> prepend, but added NO
  wasm bindings. The COMMITTED web wasm (pico_view.js) was STALE — 0 refs to
  transcript_base_seq, predating bd-72198a entirely.

## After state

- caco-picophony/src/wasm.rs: new `history_line(before_seq: u64, limit: u32)`
  static binding (serializes HostRequest::History via request_line).
- Regenerated the committed wasm (`nix develop .#caco-web-wasm --command just
  pico-web-wasm`) — now exposes history_line + transcript_base_seq + the in-core
  HistoryChunk apply_line consumption (fixes the stale wasm, bd-b11d95 guard).
- pico-view-adapter.js: `historyLine(beforeSeq, limit)` (BigInt-converts the u64).
- app.js: `picoRequestOlderHistory()` (sends the request seeded from the snapshot
  `transcript_base_seq` cursor; guarded against concurrent/eof requests; graceful
  8s timeout), prepend detection in `applyPicoSharedLine` (cursor drop), a
  "Load older history" affordance row + `picoMaybeLoadOlderOnScroll` scroll-to-top
  lazy trigger, and a scrollHeight-delta scroll ANCHOR so prepended older items
  don't jump the operator's reading position. Disconnect resets the state.
- style.css: `.pico-load-older` pill button + loading state (Nord-consistent,
  var(--radius-pill), :focus-visible).
- Consumption is automatic: `apply_line` (wasm) -> `prepend_history` (in-core,
  rejects stale/overlapping pages), so the JS only drives the REQUEST + anchor.
- Backward-compatible: an older daemon ignores the request (no chunk; pending
  clears on the timeout).

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- Files: crates/caco-picophony/src/wasm.rs, crates/caco-web/static/{app.js,
  pico-view-adapter.js,pico_view.js,pico_view_bg.wasm,style.css},
  crates/caco-web/src/tests.rs. 211 insertions / 7 files.
- Tests: +1 needle guard (pico_web_consumes_history_pagination_bd_5bdc72) across
  wasm/adapter/app.js/affordance/anchor. Existing pico-pane live scenario CLEAN
  (58 results) — regression-checked with the regenerated wasm.
- Validation: regen compiled caco-picophony->wasm clean; caco-web --lib queued.

## Operator-takeaway

The web pico pane can now page back through older transcript history (scroll to
top or click "Load older history"), reaching parity with the Android consumer and
completing another client of the bd-ce6b0d cross-client pagination. The heavy
lifting (protocol, host retention, in-core prepend) was the landed bd-72198a; this
slice is the thin wasm binding + regen + the web request/affordance/anchor.
FOLLOW-ON: a dedicated deterministic live mock subscenario in caco-web-observe
(mock a HistoryChunk response, assert older items prepend in order + cursor
advances + no scroll-jump) — the gold-standard behavioral test; this slice ships
statically-guarded + regression-checked.

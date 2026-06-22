# Session summary — bd-821adf: deterministic live mock subscenario for web /session history pagination

## Goal

Complete the gold-standard behavioral test deferred from bd-5bdc72 (web pico
/session history-pagination, LANDED cd2a0f31e6): a deterministic live mock
pico-pane subscenario in caco-web-observe that proves the load-older flow
end-to-end in a real browser, not just statically (needle guard) + regression.

## Bead(s)

- `bd-821adf` — pico /session web history pagination: deterministic live mock
  subscenario (filed + claimed + closed this session). Follow-on of bd-5bdc72.

## Before state

- The web load-older feature (bd-5bdc72) shipped needle-guarded + regression-
  checked, but had NO behavioral test exercising the actual request -> HistoryChunk
  -> prepend flow in the browser.

## After state

- caco-web-observe.rs mock /session server gains a `history_chunk` response gated
  on a `history_seen` flag (mirrors the existing AgentEnd<-steer / pi_exited<-compact
  client-driven send pattern, bd-04d343): on receiving the client's
  `{"kind":"history",...}` line it sends the older-history page.
- `mock_history_pagination_frames()`: a Snapshot with `transcript_base_seq:3`
  (2 recent items; 3 older exist beyond the bounded window) + a `history_chunk`
  (base_seq:0, 3 older items, has_more:false) that contiguously prepends.
- `run_pico_history_pagination_subscenario` + `PICO_HISTORY_PAGINATION_ASSERT_EVAL`
  (DOM-only, since picoState is module-scoped): asserts the "Load older history"
  affordance renders (transcript_base_seq>0) -> click -> bubbles GROW (older
  prepended) + affordance HIDES (cursor -> 0) + the OLDEST item lands at the top.
- Registered in run_pico_pane_scenario; the full pico-pane scenario now runs 59
  results (was 58), 2/2 deterministic PASS.

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- Files: crates/caco-web/src/bin/caco-web-observe.rs only (mock history_chunk
  support + frames + subscenario + eval + registration).
- Tests: +1 live subscenario (deterministic, 2/2). Behavioral end-to-end proof
  of bd-5bdc72's load-older feature.
- Gotcha fixed: the eval's CSS selector must use LITERAL double-quotes inside the
  Rust raw string (`[data-pico-action="load-older"]`), not backslash-escaped
  (`\"`), or querySelector silently matches nothing.

## Operator-takeaway

The web load-older history-pagination feature is now behaviorally proven, not
just statically guarded: a deterministic mock answers the client's history request
with an older page and the test asserts the prepend + cursor-advance + affordance-
hide + scroll behavior. This closes the bd-ce6b0d cross-client pagination's web
client to full test parity with its shipped behavior.

# Session summary — caco-web: incremental streaming patch (no per-chunk flicker) (bd-30f927)

## Goal

Operator-flagged (Harry) P2: every streamed chunk in the caco-web agent-session
("pico") transcript triggered a full re-render/reflow of the ENTIRE transcript,
making it visibly flicker/flash per chunk. Fix: patch only the active streaming
bubble in place for the common text/thinking-extend case.

## Bead(s)

- `bd-30f927` — caco-web: streamed chunks trigger full feed redraw/flicker (claimed + fixed)

## Before state

- Failing tests: none.
- Root cause (code-traced + live-DOM confirmed): applyPicoEvent (app.js) handled
  every TextDelta/ThinkingDelta by picoPushBlock + schedulePicoRender ->
  renderPicoSnapshot, which rebuilds the WHOLE transcript and does
  `host.innerHTML = rows.join('')` — a full re-parse/reflow per chunk (the
  bd-7e561e comment notes it resets scrollTop per frame). That full innerHTML
  replace per chunk is the flicker.

## After state

- Failing tests: none (`cargo test -p caco-web --lib`, tj-9e17e5ad + new contract
  test `pico_streaming_incremental_tail_patch_bd_30f927`).
- Fix: a same-kind streaming text/thinking EXTEND now patches ONLY the active
  streaming tail bubble's body in place (new `picoPatchStreamingTail`), tagged with
  `id="pico-streaming-tail"` + `data-pico-tail-kind`. picoPushBlock returns whether
  it extended the same-kind tail; applyPicoEvent takes the incremental path for a
  same-kind extend and falls back to the full renderPicoSnapshot for new blocks /
  finalize / tools / structural changes / when the tail node is absent (safe).
- Live-DOM validated (PLAYWRIGHT_MCP_EXECUTABLE_PATH=nix chromium, mock-injected a
  streaming turn via applyPicoEvent): AgentStart + first TextDelta = 2 full renders
  (structural, necessary); the next 3 TextDelta EXTENDS took the incremental patch
  (full-render count stayed at 2, incrementalTailPatches 1->2->3) with the tail
  content exactly correct ("Hello" -> ... -> "Hello world streaming smoothly.").
  Before the fix all 5 would full-render (flicker); after, the extends do not.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js`: picoBubble +bodyAttr param; picoPushBlock returns
    extended; new picoPatchStreamingTail; renderPicoSnapshot tags the tail bubble;
    applyPicoEvent takes the incremental path for same-kind extends.
  - `crates/caco-web/src/tests.rs`: +1 contract test (bd-30f927).
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: streaming agent-session output no longer flickers per chunk
  (incremental tail patch); structural changes + correctness + scroll-anchoring
  preserved; non-streaming rendering unchanged.

## Operator-takeaway

The pico transcript flicker was a full host.innerHTML replace on every streamed
chunk. The fix patches only the streaming tail bubble in place for the common
text-extend case (full render reserved for structure), validated end-to-end via
mock-injected streaming + the render-count metric — not assumed. Safe-by-
construction (falls back to full render whenever uncertain).

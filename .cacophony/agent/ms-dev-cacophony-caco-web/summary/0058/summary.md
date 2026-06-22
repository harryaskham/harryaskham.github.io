# Session summary — bd-6b0f19: workspace.js visibility-gated refresh

## Goal

Continue the caco-web perf polish loop. Audited setInterval
callbacks for hidden-tab perf leaks. Found 2 in workspace.js
that fire even when the tab is hidden, missing the same
visibility guard that bd-aba11f / bd-f8ae0d added to app.js.

## Bead(s)

- `bd-6b0f19` — [caco-web] gate workspace.js bead/agent refresh intervals on visibilityState

## Before state

```js
function scheduleRefreshes() {
    beadRefreshTimer = setInterval(() => loadBeads().catch((err) => { ... }), 10000);
    agentRefreshTimer = setInterval(() => loadAgents().catch((err) => updateStatus(`agent refresh failed: ${err.message}`, true)), 5000);
}
```

For an operator with the workspace view open in a
backgrounded tab: 6 req/min loadBeads + 12 req/min
loadAgents = 18 req/min wasted on a tab they're not even
looking at.

## After state

```js
function scheduleRefreshes() {
    beadRefreshTimer = setInterval(() => {
        if (document.visibilityState !== 'visible') return;
        loadBeads().catch((err) => { ... });
    }, 10000);
    agentRefreshTimer = setInterval(() => {
        if (document.visibilityState !== 'visible') return;
        loadAgents().catch((err) => updateStatus(`agent refresh failed: ${err.message}`, true));
    }, 5000);
    document.addEventListener('visibilitychange', () => {
        if (document.visibilityState !== 'visible') return;
        loadBeads().catch(() => {});
        loadAgents().catch(() => {});
    });
}
```

Same pattern as bd-aba11f / bd-f8ae0d: skip while hidden,
fire one immediate pass on visibility return so the
workspace is up-to-date the moment the operator looks.

## Why this matters

- Operator tabs spend most of their life backgrounded.
- 18 req/min on every idle hidden workspace tab adds up
  across the fleet -- battery on laptops, mobile data on
  tethered Codespaces, daemon CPU on the server.
- No user-perceptible regression: the visibility-return
  immediate-refresh keeps state fresh the moment the
  operator returns.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/workspace.js` -- scheduleRefreshes() rewritten with visibility guards + visibilitychange listener.
  - `crates/caco-web/src/tests.rs` -- regression test pins guard count >= 3, listener registration, both refresh surfaces inside the listener block, and bare-setInterval antipattern absence.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 464 -> 465; 11 pre-existing failures on main unchanged.

## Operator-takeaway

The workspace view no longer wastes ~18 req/min on idle
hidden tabs. Combined with the 8 perf landings earlier in
the session and bd-0f167b's defensive a11y polish, the
dashboard's idle-tab footprint is now lean across both
render-tier work (content-visibility, rAF coalescing,
passive scroll, layout-write coalescing) AND network-tier
work (visibility-gated polling, debounced persistence). The
next-steps audit should look at the remaining setInterval at
app.js:7151 (TTS auto-refresh, 10s) and app.js:8565 (TTY
poll, 1s) for similar treatment.

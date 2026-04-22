# Session summary — bd-f8ae0d: caco-web snapshot poll suspends when tab hidden

## Goal

caco-web's 60s snapshot poll runs unconditionally even when the tab is in the background. Most operator tabs spend their life hidden. Suspend the poll while `document.visibilityState !== 'visible'` and refetch once on visibilitychange back to 'visible'. Saves ~300KB / minute / idle tab on both wire and server.

## Bead(s)

- `bd-f8ae0d` — `[bd-ecf1a0 follow-up] caco-web tab-hidden snapshot poll backoff via document.visibilityState`. Web slice; android sister stays separate.

## Before state

- `crates/caco-web/static/app.js` line ~95 had:
  `setInterval(() => { if (state.connected) loadSnapshot({ silent: true }); }, 60000);`
- That fired regardless of `document.visibilityState`. A backgrounded operator tab still pulled the full snapshot every 60s.
- No test pinned the polling cadence.

## After state

- The 60s `setInterval` callback now also checks `document.visibilityState === 'visible'` before calling `loadSnapshot({ silent: true })`.
- A new `visibilitychange` listener fires one immediate `loadSnapshot({ silent: true })` when the tab returns to visible (so the page is fresh the moment the operator looks at it again).
- `cargo test -p caco-web --lib`: 54 pass (including 2 new content-assertions).
- `cargo clippy -p caco-web --all-targets -- -D warnings`: clean.

## Diff summary

- `crates/caco-web/static/app.js` — `+15 / -1` around the snapshot poll wiring (in the DOMContentLoaded block).
- `crates/caco-web/src/tests.rs` — `+18 lines` adding `app_js_suspends_snapshot_poll_when_tab_hidden`, which content-asserts both the `document.visibilityState === 'visible'` gate and the `visibilitychange` listener so a future refactor that re-introduces the always-on poll lights up loudly.
- Commit: `<TBD>`.

## Operator-takeaway

Cheap savings on the snapshot endpoint, especially for operators who keep many tabs open. Pair-bead bd-c70e2e (ETag + 304) is in flight under msd-4 and bd-dac14b (daemon-side memoisation) is unclaimed — together they form the tier-2 snapshot-cost reductions.

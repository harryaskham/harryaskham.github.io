# Session summary — bd-2dab6f: aria-live polite on AI-expansion loading-state

## Goal

Audit `<div class="loading-state" role="status">`
templates for the established `aria-live="polite"`
pattern. Found 8 such templates; 7 had aria-live, 1
didn't.

## Bead(s)

- `bd-2dab6f` — [caco-web] add aria-live="polite" to AI-expansion loading-state

## The outlier

```js
// app.js:4622 (before)
preview.innerHTML = '<div class="loading-state" role="status">
  <div class="loading-spinner loading-spinner-lg" aria-hidden="true"></div>
  <span>AI is structuring your beads…</span>
</div>';
```

This is the "AI is structuring your beads…" spinner
shown during quick-bead AI expansion. **bd-eb581e
explicitly allows a 30-60s wait** for the LLM to
structure free-text into multiple beads.

**For 30-60s, a screen-reader user heard nothing.** Their
focus moved into a region with no accessible name
announcement; they had no audible feedback that anything
was happening.

## Why role="status" alone is insufficient

`role="status"` is an aria-live=polite implicit container
in the WAI-ARIA spec — but many assistive-tech setups
(particularly Windows screen readers + browser combos)
treat it as a hint, not as guaranteed announcement on
DOM mutation. The 7 sibling templates uniformly hedge
by declaring `aria-live="polite"` explicitly. This 8th
outlier was the inconsistent one.

## Sibling templates (the established pattern)

| Site | Wait copy |
|---|---|
| app.js:4347 | "Loading…" (bead detail modal) |
| app.js:4622 | "AI is structuring your beads…" ← **this** |
| app.js:9174 | "Loading summaries…" |
| app.js:9208 | "Loading summary…" |
| app.js:9466 | "Loading artefacts…" |
| app.js:9505 | "Loading artefact…" |
| app.js:9531 | "Loading logs…" |
| app.js:9557 | "Loading diff…" |

All other 7 had `aria-live="polite"`. The 8th now does too.

## Test design

Two layers:

1. **Positive assertion** — full target template string
   via `format!()` concatenation per bd-5e0030
   defense-in-depth (now the established pattern for
   forward-guards and template-string assertions).
2. **Forward-guard walker** — finds every
   `<div class="loading-state" role="status">` opening
   tag in app.js and refuses any without `aria-live=`
   in the tag body. Future template additions MUST
   include the attribute.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- 1 attribute addition.
  - `crates/caco-web/src/tests.rs` -- regression test with positive + walker forward-guard.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 484 -> 485; 11 pre-existing failures on main unchanged.

## Coordination note

bd-29e9b1 (`.summaries-row` content-visibility, landed
at `ae5e62dd3d` earlier in this session) -- close call
failed due to helsinki beads-primary backpressure
(transient infrastructure issue, unrelated to the work
itself; the work IS on main). Will close when helsinki
recovers. Documenting here so the open bead status row
isn't misread as in-flight work.

## Operator-takeaway

Screen-reader users running the AI quick-bead expansion
(potentially a 30-60s wait) now hear "AI is structuring
your beads…" when the spinner appears, instead of
silence. All 8 of 8 loading-state templates in the
dashboard now uniformly announce their wait copy.

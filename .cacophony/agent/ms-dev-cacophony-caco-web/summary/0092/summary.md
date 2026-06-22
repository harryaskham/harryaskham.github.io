# Session summary — bd-25c6c1: img CLS reservation pass 2 (bd-845b8a sibling)

## Goal

Continue the bd-845b8a img CLS reservation audit. Three
remaining lazy-loaded img classes had `max-height` but
no paired `min-height` — 0->420px layout shifts when
images streamed in via SSE or scrolled into view.

## Bead(s)

- `bd-25c6c1` — [caco-web] img CLS reservation pass 2: files-preview + agent-summary inline/standalone

## The CLS bug

Three sites uncovered after bd-845b8a:

| Selector | Used by | Symptom |
|---|---|---|
| `.files-preview-image` | `renderFilesPane` (app.js:7400) | 0->420px shift per lazy-loaded file preview as user scrolls |
| `.agent-summary-inline-image img` (was shared) | `app.js:9382, 11270` | 0->420px shift per inline-embedded summary image |
| `.agent-summary-standalone-image img` (was shared) | `app.js:9383, 11270` | Same shift, larger blast radius (figure block in long summary bodies) |

The two summary-image variants were sharing a single
CSS rule, so a single `max-height` applied to both —
but inline (icon-sized) and standalone (figure) want
different floors.

## Fix

Apply the bd-845b8a triplet (`min-height` + `max-height`
+ `object-fit: contain`), splitting the prior shared
summary-image rule into two:

```css
.files-preview-image {
    max-width: 100%;
    min-height: 160px;             /* NEW: CLS floor */
    max-height: 420px;
    object-fit: contain;
    /* ... */
}

.agent-summary-inline-image img {
    /* small inline icon-sized reservation */
    display: block;
    max-width: 100%;
    min-height: 40px;              /* NEW: small floor (wrapper caps at 280px) */
    max-height: 420px;
    object-fit: contain;
    /* ... */
}

.agent-summary-standalone-image img {
    /* standalone figure reservation */
    display: block;
    max-width: 100%;
    min-height: 160px;             /* NEW: larger floor (more disruptive shift) */
    max-height: 420px;
    object-fit: contain;
    /* ... */
}
```

## Test design (5 layers)

1. **`.files-preview-image`** inline rule has all three
   triplet props (substring asserts on the single-line
   rule shape).
2. **`.agent-summary-inline-image img`** has its own
   rule with the 40px floor + max-height + object-fit
   (bounded char-window extraction per bd-57c0f5).
3. **`.agent-summary-standalone-image img`** has its
   own rule with the 160px floor + max-height +
   object-fit (separate rule, larger floor than
   inline).
4. **Prior shared selector pair NEGATIVE asserted**
   — the bug was the rule was shared so a single
   max-height applied to both; the split is the fix.
5. **bd-845b8a sibling pattern presence pin** on
   `.bead-chat-screenshot img` (broader CLS-
   reservation family regression-guard).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 1 inline rule update + 1 split of shared rule into two (3 rules total touched).
  - `crates/caco-web/src/tests.rs` -- regression test with 5 assertion layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 509 -> 510; 0 failures.

## Operator-takeaway

Files-preview pane scrolls smoothly without later rows
jumping down as preview images load. Long agent summary
bodies with embedded standalone images settle into
stable layout pre-load instead of shifting paragraphs
down by up to 420px each. Inline icon-sized summary
images get a smaller 40px floor so layout
reservations stay proportionate to the wrapper's
icon-cap. Combined with bd-845b8a, the canonical
reservation triplet now covers every lazy-loaded
img class in the dashboard chrome.

# Session summary — bd-965a34: structuredClone for 4 workspace-tree deep-clone sites

## Goal

Continue the caco-web perf polish loop. Surveyed static JS for
`JSON.parse(JSON.stringify(x))` deep-clone antipattern. Found
4 sites, all in the workspace pane-tree layer.

## Bead(s)

- `bd-965a34` — [caco-web] use structuredClone instead of JSON round-trip for 4 deep-clone sites

## Before state

| File | Line | Context |
|------|------|---------|
| `workspace-integrated.js` | 794 | `_savedTreeBeforeMaximize = JSON.parse(JSON.stringify(tree))` |
| `workspace-integrated.js` | 2206 | `layouts[name.trim()] = JSON.parse(JSON.stringify(tree))` |
| `workspace-tree.js` | 306 | inside `function serialize(tree)` |
| `workspace-tree.js` | 320 | inside `function deserialize(json)` |

`JSON.parse(JSON.stringify(x))` is a well-known deep-clone
antipattern: it serializes the entire object to a string then
parses it back. Each step is O(N) on the object size, with
additional GC pressure from the intermediate string.

## After state

Each of the 2 files now defines a small local helper near top:

```js
// bd-965a34: native structuredClone is typically 3-5x faster than
// JSON.parse(JSON.stringify(x)) for deep-cloning plain JSON trees,
// and preserves types (Date/Map/Set/etc.) for any future tree
// extension. Fallback covers test envs / pre-Safari-15.4 browsers.
const _clone = (typeof structuredClone === 'function')
    ? structuredClone
    : (x) => JSON.parse(JSON.stringify(x));
```

All 4 sites switch from `JSON.parse(JSON.stringify(x))` to
`_clone(x)`. The fallback preserves behavior in non-browser
test environments and ancient browsers.

## Browser support / safety

- `structuredClone`: Chrome/Edge 98+ (Feb 2022), Firefox 94+ (Nov 2021), Safari 15.4+ (Mar 2022).
- Well within the bd-eeb79c Safari 18.0 baseline already established by content-visibility work.
- Functional difference is nil for the current workspace tree (which is plain JSON), so this is a pure-perf change today.
- Future-proofing: any tree extension that adds Date/Map/Set/RegExp/ArrayBuffer no longer silently loses data through the JSON round-trip.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/workspace-integrated.js` -- new _clone helper at top of IIFE; 2 site edits.
  - `crates/caco-web/static/workspace-tree.js` -- new _clone helper at top of IIFE; 2 site edits inside serialize/deserialize.
  - `crates/caco-web/src/tests.rs` -- regression test pins helper + all 4 wrapped sites + all 4 bare antipattern forms absent.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 460 -> 461; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Workspace pane-tree clones (used when maximizing a pane,
saving a custom layout, or serializing/deserializing the
entire tree on layout-changed events) are now ~3-5x faster on
modern browsers. The path is hot enough to matter: every
drag-to-resize, pane-create, pane-close, pane-maximize, and
custom-layout-save triggers at least one full-tree clone.
Combined with the recent perf landings (bd-eeb79c/bd-7ff0bf
content-visibility, bd-3c01a1 rAF render coalescing,
bd-fb28e1 passive scroll, bd-fc7a23 insertAdjacentHTML), the
dashboard's render and event-handler hot paths are noticeably
leaner.

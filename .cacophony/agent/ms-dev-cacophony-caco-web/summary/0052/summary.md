# Session summary — bd-fb28e1: mark 3 scroll listeners passive

## Goal

Continue the caco-web perf polish loop. Surveyed
addEventListener('scroll', ...) sites for missing
`{ passive: true }` hints. Without passive, browsers must wait
for the handler to maybe call preventDefault before scrolling
-- this is a well-known cause of scroll jank, particularly on
touch devices.

## Bead(s)

- `bd-fb28e1` — [caco-web] mark 3 scroll listeners passive

## Before state

| File | Line | Site | preventDefault? |
|------|------|------|-----------------|
| `app.js` | 546 | document capture-phase `hideContextMenu` | No |
| `workspace-log-pane.js` | 209 | log-pane follow-mode auto-unset | No |
| `workspace-panes.js` | 877 | pre/gutter scroll-sync | No |

All 3 handlers only do read/write work; none call
preventDefault. The L546 case is the most impactful -- it's
a **document-level capture-phase** listener that fires on every
scroll anywhere in the document.

The L546 site also has a subtle bug: passing `true` as the
third positional arg sets capture mode, but **per the MDN spec**,
when you pass a boolean third arg, `passive` defaults to
`false`. To use both capture and passive, you must use the
options-object form: `{ capture: true, passive: true }`.

## After state

```js
// app.js:546
document.addEventListener('scroll', () => hideContextMenu(), { capture: true, passive: true });

// workspace-log-pane.js:209
body.addEventListener('scroll', () => { /* follow-mode logic */ }, { passive: true });

// workspace-panes.js:877
pre.addEventListener('scroll', () => { gutter.scrollTop = pre.scrollTop; }, { passive: true });
```

All three listeners now declare passive intent. The browser can
start scrolling immediately without waiting for the handler.
The codebase already does this correctly elsewhere (e.g.
`app.js:3577` for the `wrap` scroll listener) -- this pass
brings the remaining 3 sites in line.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- 1 listener corrected with bd-fb28e1 rationale comment.
  - `crates/caco-web/static/workspace-log-pane.js` -- 1 listener corrected.
  - `crates/caco-web/static/workspace-panes.js` -- 1 listener corrected.
  - `crates/caco-web/src/tests.rs` -- regression test pins corrected form at each site + rejects legacy bare-true / bare-handler forms.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 458 -> 459; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Scroll responsiveness improved across:
- The whole document (context-menu auto-close listener; was a capture-phase listener firing on every scroll anywhere).
- Log panes (follow-mode auto-unset listener).
- Source-code panes (gutter scroll-sync listener).

The browser can now scroll the relevant element immediately
without waiting for the JS handler to potentially call
preventDefault. Especially noticeable on touch devices where
scroll-jank is most visible. Compounds with bd-eeb79c /
bd-7ff0bf (off-screen rows skip render) and bd-3c01a1
(input-driven re-renders coalesce) for a leaner scrolling
experience.

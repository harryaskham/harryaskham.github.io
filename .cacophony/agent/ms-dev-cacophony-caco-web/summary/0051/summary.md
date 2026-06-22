# Session summary — bd-3c01a1: rAF-coalesced search-input renders

## Goal

Continue the caco-web perf polish loop. Audited `oninput=`
handlers and found 7 search/filter inputs that triggered full
re-renders on every keystroke with no coalescing.

## Bead(s)

- `bd-3c01a1` — [caco-web] coalesce 7 search-input render calls via requestAnimationFrame

## Before state

| Line | Input | Per-keystroke work |
|------|-------|--------------------|
| L433 | `agent-search` | persistAgentFilters + full renderAgents |
| L481 | `artefacts-filter-workspace` | full renderArtefactsView |
| L499 | `files-search` | full renderFilesView |
| L517 | `links-search` | full renderLinksView |
| L535 | `bead-search` | persistBeadFilters + full renderBeads |
| L588 | `feed-filter` | full renderFeed |
| L779 | `logs-filter` | full filterLogs |

For instances with hundreds of agents/beads/logs/feed-entries,
typing a 7-character query triggered 7 full dataset iterations
and 7 innerHTML rebuilds. bd-eeb79c / bd-7ff0bf reduced
*per-row* render cost via content-visibility, but total
iteration cost still scaled linearly per keystroke.

## After state

New `scheduleRender(fn)` helper in `app.js` (exposed on
`window`):

```js
const _coalescedRenders = new Map();
function scheduleRender(fn) {
    if (typeof fn !== 'function') return;
    if (typeof requestAnimationFrame !== 'function') { try { fn(); } catch (err) { console.error('scheduleRender:', err); } return; }
    const pending = _coalescedRenders.get(fn);
    if (pending != null) cancelAnimationFrame(pending);
    const id = requestAnimationFrame(() => {
        _coalescedRenders.delete(fn);
        try { fn(); } catch (err) { console.error('scheduleRender:', err); }
    });
    _coalescedRenders.set(fn, id);
}
window.scheduleRender = scheduleRender;
```

Each of the 7 input sites wraps its expensive render in
`scheduleRender(...)`:

```html
oninput="persistAgentFilters();scheduleRender(renderAgents)"
oninput="scheduleRender(renderArtefactsView)"
oninput="scheduleRender(renderFilesView)"
oninput="scheduleRender(renderLinksView)"
oninput="persistBeadFilters();scheduleRender(renderBeads)"
oninput="scheduleRender(renderFeed)"
oninput="scheduleRender(filterLogs)"
```

Persistence calls preserved as immediate (they're cheap and
localStorage persistence wants to fire frequently). Only the
expensive renderXxx/filterXxx call is coalesced.

## Why rAF, not setTimeout

- **rAF** runs at the browser's repaint cadence (~16ms at 60Hz). Coalesced renders feel instant -- there's no perceptible lag because the user couldn't see a render before the next frame anyway.
- **setTimeout(..., 150)** introduces a real 150ms perceived delay.
- **rAF auto-pauses** on hidden tabs (browser stops firing the callback) -- background tabs don't waste cycles.
- **Keyed by function identity via Map**: two different render functions don't cancel each other (renderAgents pending doesn't drop renderBeads pending).
- **Graceful fallback**: if rAF unavailable (e.g. test environment), helper falls back to immediate invocation.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- new 17-line scheduleRender helper near top with bd-3c01a1 rationale comment.
  - `crates/caco-web/static/index.html` -- 7 oninput handler edits.
  - `crates/caco-web/src/tests.rs` -- regression test pins helper present + each of 7 wrapped sites present + bare unwrapped forms absent.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 457 -> 458; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Typing in any of the 7 search/filter inputs (agents, beads,
logs, feed, files, links, artefacts) now collapses rapid
keystroke bursts into one render per repaint frame instead of
one render per keystroke. For large datasets, this should be a
multi-x reduction in input-handler CPU cost without any
perceptible lag (rAF feels instant). bd-eeb79c / bd-7ff0bf
content-visibility wins compound: off-screen rows skip render
work AND on-screen list re-renders happen less often.

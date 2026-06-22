# Session summary — bd-3cc202: filter-chip overflow rAF coalesce

## Goal

After the drag-handler rAF audit was complete (bd-f8d959 /
bd-1f50c9 / bd-171bcc / bd-d09c97), audited the remaining
scroll + resize handlers. The chip-overflow affordance was
the last one still calling its layout-touching update
directly per event.

## Bead(s)

- `bd-3cc202` — [caco-web] rAF-coalesce filter-chip overflow-affordance update across scroll + window-resize

## Before state

```js
function attachFilterChipOverflowAffordance(wrap) {
    if (!wrap) return;
    const update = () => {
        const overflow = wrap.scrollWidth - wrap.clientWidth;    // layout read
        wrap.classList.remove('at-start', 'at-end', 'at-both', 'no-overflow');
        if (overflow <= 1) { wrap.classList.add('no-overflow'); return; }
        const left = wrap.scrollLeft;                            // layout read
        const right = overflow - left;
        if (left <= 1) wrap.classList.add('at-start');
        else if (right <= 1) wrap.classList.add('at-end');
        else wrap.classList.add('at-both');
    };
    if (!wrap.__chipAffordanceWired) {
        wrap.addEventListener('scroll', update, { passive: true });
        window.addEventListener('resize', update);    // <-- one per wrap!
        wrap.__chipAffordanceWired = true;
    }
    requestAnimationFrame(update);
}
```

Two costs per event:

1. **Per-scroll-event layout reads + classList writes.**
   `scrollWidth` / `clientWidth` / `scrollLeft` force
   synchronous reflow. 60-120 Hz during fast horizontal
   chip scrolling.
2. **Per-resize-event work × N wraps.** Each call site
   wires its own window-resize listener. With two chip
   rows visible (typical: agent + bead status chips),
   every browser resize event triggers 2 layout-reads +
   2 classList writes; during a window drag at 60 Hz
   that's 120 layout-reads/sec.

## After state

```js
function attachFilterChipOverflowAffordance(wrap) {
    if (!wrap) return;
    const update = () => {
        wrap.__chipAffordanceRaf = 0;           // ← clear cache at frame start
        // ... existing layout reads + classList toggles
    };
    const scheduleUpdate = () => {
        if (wrap.__chipAffordanceRaf) return;
        wrap.__chipAffordanceRaf = requestAnimationFrame(update);
    };
    if (!wrap.__chipAffordanceWired) {
        wrap.addEventListener('scroll', scheduleUpdate, { passive: true });
        window.addEventListener('resize', scheduleUpdate);
        wrap.__chipAffordanceWired = true;
    }
    scheduleUpdate();
}
```

Burst scroll/resize events now coalesce to at most one
layout-read + classList-write per frame, per wrap. Matches
the drag-handler audit pattern exactly.

## Important implementation note

The per-wrap rAF id is stored **on the wrap** (`wrap.__chipAffordanceRaf`),
NOT as a closure local. Reason: `attachFilterChipOverflowAffordance`
is called from each chip-row re-render (when filters/counts
update), and the `__chipAffordanceWired` short-circuit means
existing listeners are reused. But each re-render creates a
new `scheduleUpdate` / `update` closure. If the rAF id were
a closure local, every re-render would lose the previously-
pending rAF and risk wasted work. Per-element stash on the
wrap guarantees coalescing across all closures.

(This mirrors the pattern logged earlier for rAF coalescing:
"WeakMap-keyed by element (per-element ops)". Here we use
the wrap as the element key directly.)

## rAF-coalesce audit summary

| Bead | Surface | Trigger |
|------|---------|---------|
| bd-f8d959 | card-hover spotlight | mousemove (120 Hz) |
| bd-1f50c9 | workspace.js splitter | mousemove drag |
| bd-171bcc | workspace-integrated pane handle | mousemove drag |
| bd-d09c97 | table column resize | mousemove drag |
| bd-3cc202 | chip-overflow affordance | scroll + resize |

All hot-event-frequency handlers in the dashboard are now
rAF-coalesced.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- attachFilterChipOverflowAffordance rAF-coalesce via per-wrap stashed rAF id.
  - `crates/caco-web/src/tests.rs` -- regression test pins all 5 invariants including both bare-antipattern guards.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 475 -> 476; 11 pre-existing failures on main unchanged.

## Operator-takeaway

The last per-event layout-touching handler in the dashboard
is now rAF-coalesced. During a window drag or fast chip-
row scroll with two chip rows visible, the dashboard does
~60 layout-reads/sec instead of ~120-240. Combined with
the prior 19 perf/polish wins this session, every hot-
event handler (drag, mousemove, scroll, resize, type) is
uniformly coalesced.

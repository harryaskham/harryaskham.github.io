# Session summary — bd-b702b4: rAF-coalesce cluster-pulse canvas resize

## Goal

Follow-up to bd-4b1949 (terminal RO coalescing). The
cluster-pulse hero canvas had the SAME unguarded
pattern on TWO listeners.

## Bead(s)

- `bd-b702b4` — [caco-web] rAF-coalesce cluster-pulse canvas resize (RO + window listener)

## The perf bug

Two unguarded resize listeners on the cluster-pulse
canvas (`app.js:12305-12312`):

```js
if ('ResizeObserver' in window) {
    sym.ro = new ResizeObserver(() => resize());      // unguarded RO
    sym.ro.observe(sym.canvas);
}
window.addEventListener('resize', resize);             // unguarded window resize
```

Where `resize()`:

```js
function resize() {
    if (!sym.canvas) return;
    const rect = sym.canvas.getBoundingClientRect();   // forces layout sync
    sym.dpr = Math.min(2, window.devicePixelRatio || 1);
    sym.canvas.width  = Math.max(1, Math.floor(rect.width  * sym.dpr));
    sym.canvas.height = Math.max(1, Math.floor(rect.height * sym.dpr));
}
```

Setting `canvas.width`/`height`:

- Clears the canvas
- **Reallocates the GPU backing store**
- Per HTML spec, this is the expensive canvas reshape path

`getBoundingClientRect()` forces a layout sync.

During splitter drag or window resize, BOTH listeners
fire at ~60Hz — so up to **2N fires per frame**.

The `prefers-reduced-motion` throttle in `tick()` (12fps
cap when reduced-motion is set) **doesn't protect
resize** — separate listener path.

## Fix

Coalesce helper inside the IIFE:

```js
function scheduleResize() {
    if (sym.resizeRaf) return;
    sym.resizeRaf = requestAnimationFrame(() => {
        sym.resizeRaf = 0;
        resize();
    });
}
```

Route both listeners through it:

```js
sym.ro = new ResizeObserver(scheduleResize);
window.addEventListener('resize', scheduleResize);
```

**Mount-time direct `resize()` calls** in `start()` and
`mountTo()` stay direct — coalescing applies only to
event-driven storms, so the initial canvas size is set
synchronously and the first paint isn't deferred a frame.

## Test design (5 layers)

1. **Positive scheduleResize helper signature** via
   `format!()` concat per bd-5e0030.
2. **Positive RO callback signature** + **stale-reference
   refusal** (negative assertion: the unguarded
   `() => resize()` form must be gone).
3. **Positive window-resize listener signature** +
   stale-reference refusal.
4. **Mount-time direct `resize()` count floor** (`>= 2`)
   so a later refactor that wraps EVERY `resize()`
   call — including mount-path — in the coalesce
   wrapper fails clearly.
5. **bd-4b1949 sibling pattern presence pin** —
   workspace.js `let resizeRaf = 0` + app.js
   `if (ttyState.resizeRaf) return;` must remain
   (broader resize-coalesce family regression-guard).

## Two-cycle compound

| Cycle | Bead | Surface |
|---|---|---|
| Prev | bd-4b1949 | xterm terminal hosts (workspace.js + app.js ttyState) |
| This | bd-b702b4 | cluster-pulse hero canvas (RO + window listener) |

Together: all three high-frequency resize-storm sites
in the dashboard are now rAF-coalesced.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- 13-line scheduleResize helper + 2 listener-wiring updates.
  - `crates/caco-web/src/tests.rs` -- regression test with 5 assertion layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 490 -> 491; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Resizing the window or dragging the splitter no longer
reallocates the cluster-pulse canvas GPU backing store
60+ times per second. Backing store now reshapes at
most once per frame. Initial paint timing unchanged.

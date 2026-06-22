# Session summary — bd-705d56: updateResultCount threshold-only across 9+ aria-live badges

## Goal

Third sibling in the threshold-only live-region family
(bd-98e4ea freshness-indicator, bd-432c3e connection-
status). Scale the pattern to the 9+ result-count
badges in the dashboard chrome.

## Bead(s)

- `bd-705d56` — [caco-web] updateResultCount threshold-only DOM mutation across 9+ aria-live badges

## The a11y noise bug

`updateResultCount(elemId, shown, total)` is THE
function that updates all 9+ result-count badges:

```
agents-count, beads-count, services-count, chat-count,
feed-count-badge, inbox-count, notifications-count,
projects-count, choices-count, links-count, files-count,
artefacts-count
```

Every one has `aria-live="polite"` in `index.html`. And
the function is called from render functions on every
snapshot:

| Caller (line) | Badge |
|---|---|
| `renderAgents` (3505) | `agents-count` |
| `renderBeads` (3804) | `beads-count` |
| `renderFeed` (5439) | `feed-count-badge` |
| `renderChat` (6155, 6166) | `chat-count` |
| `renderServices` (7255) | `services-count` |
| `renderNotifications` (7786) | `notifications-count` |
| `renderProjects` (11335) | `projects-count` |
| `renderInbox` (11590) | `inbox-count` |
| `renderChoices` (11643) | `choices-count` |

On a steady-state dashboard each snapshot rewrote all
badges even when `(shown, total)` was byte-identical:

```js
el.textContent = String(total);                  // → SR announces
el.classList.add/remove('result-count-filtered');
el.setAttribute/removeAttribute(role/tabindex/title);
el.onclick = () => clearFiltersFor(elemId);     // fresh closure per call
```

Effect on SR users: `"5"`, `"5"`, `"5"` repeatedly
announced for every badge as snapshots flow in.

## Fix

Scale the bd-98e4ea / bd-432c3e single-sig pattern to a
Map keyed by `elemId`:

```js
function updateResultCount(elemId, shown, total) {
    const el = document.getElementById(elemId);
    if (!el) return;
    const key = `${shown}|${total}`;
    if (!state.resultCountSigs) state.resultCountSigs = new Map();
    if (state.resultCountSigs.get(elemId) === key) return;
    state.resultCountSigs.set(elemId, key);
    // ... existing if/else ...
}
```

**Safety**: badge `<span>` elements are static in
`index.html` (never recreated, only mutated), so
previously-set `role` / `tabindex` / `title` / `onclick`
persist on the element when we skip. The cache is
correct.

## Test design (7 layers)

1. `state.resultCountSigs` **Map lazy-initialized**.
2. **Cache key** `${shown}|${total}` via `format!()`
   concat per bd-5e0030.
3. **Early return on cache hit**.
4. **Cache write BEFORE mutation block** — bounded
   char-window verification per bd-57c0f5 pattern.
5. **Existing `clearFiltersFor` click wiring
   preserved** (no logic regression).
6. **bd-98e4ea + bd-432c3e sibling pattern presence
   pins** (`state.lastFreshnessSig`,
   `state.lastConnectionRenderSig`) — broader
   threshold-only family regression-guard.
7. **`aria-live="polite"` spot-check** on 3
   high-traffic badges in `index.html`.

## Three-cycle compound (a11y threshold-only)

| Cycle | Bead | Live region | Scope |
|---|---|---|---|
| Prev-2 | bd-98e4ea | `#freshness-indicator` | single sig |
| Prev-1 | bd-432c3e | `#connection-status` | single sig |
| This | bd-705d56 | **9+ result-count badges** | Map-keyed |

ALL aria-live regions in the dashboard chrome that fire
on snapshots are now threshold-only.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- ~8-line addition at the top of `updateResultCount`.
  - `crates/caco-web/src/tests.rs` -- regression test with 7 assertion layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 505 -> 506; 0 failures.

## Operator-takeaway

NVDA/VoiceOver/JAWS users on a healthy dashboard no
longer hear the same `5`, `30`, `8` counts re-announced
for every result badge on every snapshot. Combined with
the prev two cycles, every aria-live region in the
dashboard chrome that fires on snapshots is now
threshold-only — only speaks on real value changes.

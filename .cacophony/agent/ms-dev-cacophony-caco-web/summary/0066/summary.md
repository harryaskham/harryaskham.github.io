# Session summary — bd-42115c: agent-logs filter debounce + regex hoist

## Goal

After the command palette debounce (bd-5e0030), audited
other text-input handlers for the same per-keystroke-render
pattern. Found the agent-logs filter with an additional
hot-spot issue: regex compilation INSIDE a per-line map.

## Bead(s)

- `bd-42115c` — [caco-web] debounce agent-logs filter input + hoist per-line regex out of map

## Before state

```js
function filterAgentLogs() {
    // ...
    const lines = agentDetailState.logs.split('\n');
    const matched = lines.filter(l => l.toLowerCase().includes(query));
    if (counter) counter.textContent = `${matched.length} of ${lines.length} lines`;
    container.innerHTML = matched.map(l => {
        const escaped = escapeHtml(l);
        const re = new RegExp(`(${query.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')})`, 'gi');  // <-- INSIDE the map!
        return escaped.replace(re, '<mark class="log-highlight">$1</mark>');
    }).join('\n');
}
```

```html
<input ... id="agent-logs-search" ... oninput="filterAgentLogs()" ...>
```

Three layered perf issues:

1. **Per-keystroke firing (no debounce).** Every input
   event triggered the whole pipeline. For MB-scale agent
   logs, each keystroke = re-split + re-filter + re-render.

2. **Regex compiled per matched line.** This is the real
   hot spot. The pattern depends only on `query` (shared
   across all matched lines), but the construction was
   inside the `.map()` callback. For 10k matched lines
   that's **10k RegExp allocations + 10k compilations per
   keystroke**. Pure waste.

3. **Full innerHTML rebuild per keystroke.** Compounds
   with #1.

## After state

```js
function filterAgentLogs() {
    // ...
    const lines = agentDetailState.logs.split('\n');
    const matched = lines.filter(l => l.toLowerCase().includes(query));
    if (counter) counter.textContent = `${matched.length} of ${lines.length} lines`;
    // bd-42115c: hoist regex compilation out of the per-line .map().
    const highlightRe = new RegExp(`(${query.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')})`, 'gi');
    container.innerHTML = matched.map(l =>
        escapeHtml(l).replace(highlightRe, '<mark class="log-highlight">$1</mark>')
    ).join('\n');
}

let _filterAgentLogsDebounceId = 0;
function scheduleFilterAgentLogs() {
    if (_filterAgentLogsDebounceId) clearTimeout(_filterAgentLogsDebounceId);
    _filterAgentLogsDebounceId = setTimeout(() => {
        _filterAgentLogsDebounceId = 0;
        filterAgentLogs();
    }, 60);
}
window.scheduleFilterAgentLogs = scheduleFilterAgentLogs;
```

```html
<input ... id="agent-logs-search" ... oninput="scheduleFilterAgentLogs()" ...>
```

Two layered fixes:

1. **Hoist the regex out of `.map()`.** Single RegExp per
   filter operation. O(matched-lines) RegExp allocations
   collapses to O(1). For 10k matched lines, that's a
   ~10000x reduction in regex work per keystroke.

2. **Debounce the input event at 60ms** via the new
   `scheduleFilterAgentLogs()` trampoline. Same pattern as
   bd-5e0030. Coalesces burst typing while feeling instant.

The unwrapped `filterAgentLogs()` is preserved for any
non-debounced caller that wants immediate application
(e.g. after a programmatic logs update). Currently only
the inline oninput= calls it, but keeping the API stable
is the right shape.

## Why this matters

Agent logs viewing is a common operator workflow during
incident investigation. Filtering down to specific patterns
(e.g. "ERROR", "panic", "timeout") is the primary use case.
With the old code, typing "panic" (5 chars) in a 10k-line
log = 5 × (10k filter scans + 10k RegExp allocations +
10k regex compilations + 10k regex applies + 1 innerHTML
rebuild). Now: 1 debounced pass with 1 RegExp.

## Test gotcha (logged for future)

Asserting "regex declared BEFORE the .map() call" needed a
positional check (`hoist_pos < map_pos`), not just two
independent `contains()` checks, because the original
antipattern would also satisfy a naive presence test.
**Lesson:** when "ordering" matters in source layout, the
forward-guard test must compare byte positions.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- filterAgentLogs regex hoisted out of .map(); new scheduleFilterAgentLogs trampoline; inline oninput= on #agent-logs-search updated to use the trampoline.
  - `crates/caco-web/src/tests.rs` -- regression test pins all 5 invariants including the positional check.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 472 -> 473; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Filtering large agent logs is now substantially faster:
~10000x fewer RegExp allocations per keystroke (for 10k
matched lines), and burst typing is coalesced via 60ms
debounce. Combined with the prior 16 perf wins this
session, the per-keystroke / per-hover / per-mousemove
hot paths across the dashboard are uniformly lean.

# Session summary — bd-c50ae9: workspace search-input debounces

## Goal

Continue the text-input debounce pass started in bd-5e0030
(command palette) and bd-42115c (agent logs). Catch the
remaining workspace search inputs that still fire heavy
render functions per keystroke.

## Bead(s)

- `bd-c50ae9` — [caco-web] debounce workspace log-pane filter + bead-list-pane search inputs

## Before state

### workspace-log-pane.js (filter input)

```js
filterInput.addEventListener('input', () => {
    const raw = filterInput.value.trim();
    try {
        state.filterRegex = raw ? new RegExp(raw) : null;
        filterInput.classList.remove('workspace-log-pane__filter--invalid');
    } catch (_e) {
        filterInput.classList.add('workspace-log-pane__filter--invalid');
        return;
    }
    rerenderAll();                       // <-- per keystroke
});
```

`rerenderAll()` clears innerHTML, filters ENTIRE log buffer
through `lineMatches` (regex test per line), builds a
DocumentFragment of up to RENDER_CAP rows, and appends.
For thousands of buffered log lines, every keystroke =
full filter + DOM rebuild.

### workspace-bead-list-pane.js (search input)

```js
$search.addEventListener('input', () => {
    state.config.search = $search.value;
    state.cursor = 0;
    applyFilters(); renderRows();        // <-- per keystroke
});
```

`applyFilters()` iterates all loaded beads applying
status/priority/search filters; `renderRows()` rebuilds
the table body. For a busy board, every keystroke = full
filter + full table rebuild.

## After state

### workspace-log-pane.js

```js
let _filterDebounceId = 0;
filterInput.addEventListener('input', () => {
    const raw = filterInput.value.trim();
    try {
        state.filterRegex = raw ? new RegExp(raw) : null;
        filterInput.classList.remove('workspace-log-pane__filter--invalid');
    } catch (_e) {
        filterInput.classList.add('workspace-log-pane__filter--invalid');
        return;
    }
    // SYNCHRONOUS regex-compile + invalid-class stays above;
    // user gets immediate validation feedback on bad patterns.
    if (_filterDebounceId) clearTimeout(_filterDebounceId);
    _filterDebounceId = setTimeout(() => {
        _filterDebounceId = 0;
        rerenderAll();
    }, 60);
});
```

### workspace-bead-list-pane.js

```js
let _searchDebounceId = 0;
$search.addEventListener('input', () => {
    state.config.search = $search.value;
    state.cursor = 0;
    if (_searchDebounceId) clearTimeout(_searchDebounceId);
    _searchDebounceId = setTimeout(() => {
        _searchDebounceId = 0;
        applyFilters(); renderRows();
    }, 60);
});
```

Same 60ms debounce template as bd-5e0030 / bd-42115c.

## Important UX detail (preserved)

The log-pane regex-compile + invalid-class apply stays
synchronous BEFORE the debounce. Reason: the user needs
immediate visual feedback that their regex is invalid (red
border, etc.) even while the expensive rerender waits a
frame. Test pins this with a positional check (invalid-
class write position < setTimeout position) — same kind of
positional assertion lesson logged in bd-42115c.

## Text-input debounce pass complete

| Bead | Surface | Trampoline | Heavy work deferred |
|------|---------|-----------|---------------------|
| bd-5e0030 | command palette input | inline 60ms | filtered render + DOM rebuild |
| bd-42115c | agent-logs filter | scheduleFilterAgentLogs (named) | regex-applied highlight render |
| bd-c50ae9 | workspace-log-pane filter | inline 60ms (validation stays sync) | rerenderAll |
| bd-c50ae9 | workspace-bead-list-pane search | inline 60ms | applyFilters + renderRows |

Every text-input search/filter in the dashboard now uses
the same 60ms debounce template.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/workspace-log-pane.js` -- 60ms debounce for rerenderAll while keeping regex validation sync.
  - `crates/caco-web/static/workspace-bead-list-pane.js` -- 60ms debounce for applyFilters + renderRows.
  - `crates/caco-web/src/tests.rs` -- one regression test covering both panes including the log-pane positional check.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 473 -> 474; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Both workspace panes that allow live text filtering
(log-pane, bead-list-pane) now coalesce burst keystrokes
into a single render per 60ms window. Typing in either
filter feels identical (regex validity in log-pane is
still immediate via the unchanged sync path), but each
keystroke no longer triggers full filter + DOM rebuild.
Combined with the prior 17 perf wins this session, every
hot path the user touches during normal interaction
(scroll, drag, hover, type) is render-coalesced.

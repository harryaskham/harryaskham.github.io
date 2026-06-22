# Session summary — bd-2b16d9: wire Shift+P workspace shortcut

## Goal

Audit shortcuts-overlay drift (backlog item). Found a
documented-but-unimplemented shortcut affecting real
user-visible behavior.

## Bead(s)

- `bd-2b16d9` — [caco-web] implement Shift+P workspace badge shortcut

## The bug

`Shift+P` was advertised in TWO places:

1. **Workspace badge button title** (`app.js:972`):
   ```js
   title="Open ${state.workspaceProject} project view (Shift+P)"
   ```
2. **`window.KEYBOARD_BINDINGS`** registry (`app.js:1879`):
   ```js
   { keys: ['Shift+P'], desc: 'Focus workspace project badge', scope: 'Global' }
   ```

But **no handler implemented it**. Sibling shortcuts
worked fine:

| Key | Handler location | Behavior |
|---|---|---|
| `p` (bare) | line 1057 | Projects view |
| `Ctrl/Cmd+Shift+P` | line 1075 | Command palette |
| `Shift+X` | line 1297 | Clear workspace project |
| **`Shift+P`** | **(none)** | **promised, not wired** |

Users who saw the hint and pressed it got no feedback.

## Fix

Added a `Shift+P` handler right next to `Shift+X`,
matching its pre-condition shape:

```js
if (e.key === 'P' && e.shiftKey && !e.ctrlKey && !e.metaKey && !e.altKey) {
    if (state.workspaceProject) {
        e.preventDefault();
        switchView('projects');
        return;
    }
}
```

And updated KEYBOARD_BINDINGS description from
`'Focus workspace project badge'` (described a
non-existent action) to `'Open workspace project view'`
(matches both the badge title and the wired handler).

## Test design (4 layers)

1. **Positive handler signature** via `format!()`
   concatenation per bd-5e0030 defense-in-depth — full
   shape including pre-conditions, `preventDefault`,
   `switchView('projects')`, and early return.
2. **KEYBOARD_BINDINGS description** assertion +
   negative assertion that the old stale description is
   gone.
3. **Sibling Shift+X handler presence pin** — the
   matching pattern this implements against.
4. **Badge title presence pin** — the user-facing
   promise the handler now honors.

## Audit context

This was spotted during a broader shortcuts-overlay
drift audit:

- `app.js:1885` `toggleKeyboardHelp` — canonical
  keyboard-help overlay, consumes
  `window.KEYBOARD_BINDINGS`. Registered as the `?`
  handler at line 1205.
- `app.js:4435` `toggleShortcutsOverlay` — legacy
  cheat-sheet overlay, hardcodes its own list of 17
  shortcuts (drifted from KEYBOARD_BINDINGS). Registered
  as a second `?` handler at line 1306 (dead code — the
  first handler at 1205 fires first and returns).

The legacy `toggleShortcutsOverlay` + its dead `?`
handler are a bigger drift surface deserving a separate
cycle (requires updating bd-ca8a3a and bd-bfc59e tests
that pin its legacy DOM structure). This cycle ships
only the user-visible Shift+P fix.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- 10-line handler addition + 1-line description string update.
  - `crates/caco-web/src/tests.rs` -- regression test with 4 assertion layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 487 -> 488; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Pressing Shift+P with a workspace project set now opens
the Projects view, matching what the badge tooltip and
keyboard-help overlay had been promising. The
documented behavior is now the real behavior.

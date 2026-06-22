# Session summary — bd-74610b: kill dead keyboard-shortcuts overlay (handler + function + CSS rule)

## Goal

Land the long-pending dead-code cleanup from the
backlog: the legacy unreachable keyboard-shortcuts
overlay that survived after the canonical
`toggleKeyboardHelp()` system landed.

## Bead(s)

- `bd-74610b` — [caco-web] kill dead keyboard-shortcuts overlay: handler + function + CSS rule

## Three dead siblings

### Dead handler — `app.js:1318-1323`

```js
// ? to show keyboard shortcuts
if (e.key === '?' && !e.ctrlKey && !e.metaKey) {
    e.preventDefault();
    toggleShortcutsOverlay();
    return;
}
```

Unreachable because the canonical `?` handler at
`app.js:1205` (`bd-e49551`) runs first **in the same
`setupKeyboardShortcuts` function**, calls
`toggleKeyboardHelp()`, and `return`s.

### Dead function — `app.js:4464`

```js
function toggleShortcutsOverlay() { ... }
```

~38-line parallel modal builder. Only caller was the
dead handler above.

### Dead CSS — `style.css:7290`

```css
#shortcuts-overlay {
    -webkit-backdrop-filter: blur(12px);
    backdrop-filter: blur(12px);
}
```

Targets the element ID only created by the dead
function. Rule never matched.

## Why this matters

- **Bundle size**: ~40-line dead function + dead handler + dead CSS rule shipped to every dashboard load.
- **Maintenance hazard**: future contributors editing keyboard shortcuts could update the dead overlay's shortcut list and wonder why their changes don't appear.
- **A11y drift gap**: the dead overlay carried `role="dialog"` + `aria-modal="true"` + `aria-label="Keyboard shortcuts"` semantics that could drift out-of-sync with the canonical implementation. If someone ever resurrected this dead path (e.g., by changing handler order during a refactor), the AT semantics would conflict between the two overlays.

## Fix

Removed all three siblings. Replaced each with an
explanatory marker comment so a future contributor
who finds the comment in a `grep` understands WHY the
code is missing and won't re-introduce a parallel
implementation.

## Test design (7 layers)

**NEGATIVE (dead siblings removed):**
1. `function toggleShortcutsOverlay()` declaration gone.
2. Dead `?` handler body literal call+return pair gone (precise signature so unrelated future calls don't false-trip).
3. `#shortcuts-overlay {` CSS rule gone.
4. Dead CSS rule introductory comment gone (belt-and-braces).

**POSITIVE (canonical survivors remain):**
5. `function toggleKeyboardHelp() {` remains (the survivor).
6. bd-e49551 `?` handler comment marker + `toggleKeyboardHelp(); return;` body pair remain.
7. `function setupKeyboardShortcuts() {` container function remains.

Plus the `bd-74610b: dead `?` shortcut handler removed.`
replacement-marker assertion catches future regressions
that would silently re-add the parallel handler.

## Sibling test updates

Two existing tests pinned the dead overlay's
existence — updated to reflect the new survivor count:

- **bd-ca8a3a (dialog semantics floor)**: floor lowered from 5 to 4 dialog-semantic calls (`role`/`aria-modal`); the per-overlay shortcuts-overlay assertion block was converted to a NEGATIVE assertion (must remain removed).
- **bd-bfc59e (modal-close button floor)**: floor lowered from 3 to 2 dynamic modal-close buttons in `app.js`.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- dead handler removed (replaced with marker comment); dead toggleShortcutsOverlay function removed (replaced with explanatory comment); ~45 lines deleted, ~10 lines comment added.
  - `crates/caco-web/static/style.css` -- dead #shortcuts-overlay rule + intro comment removed (replaced with explanatory comment); ~6 lines deleted, ~4 lines comment added.
  - `crates/caco-web/src/tests.rs` -- new bd-74610b regression test (7 layers) + bd-ca8a3a/bd-bfc59e sibling-test updates for the new survivor count.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test (sibling tests updated, not added). Net pass count: 510 -> 511; 0 failures.

## Operator-takeaway

Dashboard bundle is smaller and the keyboard help
system has one canonical implementation instead of two
divergent ones. Future contributors who modify
keyboard shortcuts only need to edit
`toggleKeyboardHelp()` and won't confuse-edit a dead
parallel modal. Defense against re-introduction is
built into the test layer.

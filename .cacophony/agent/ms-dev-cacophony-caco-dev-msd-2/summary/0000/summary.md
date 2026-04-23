# Session summary — bd-03f759 workspace-view layout import/export (V2)

## Goal

Sidestep the share-link flow (bd-689ee0) for portable layout backup
or for moving layouts between disconnected Cacophony instances. Per
acceptance criteria: Export… / Import… menuitems on the views
dropdown, schema-version validation with helpful errors, deep-equal
round-trip, and runtime UI state stripped on import.

## Bead(s)

- `bd-03f759` — workspace-view V2 layout import/export (P2)
- (parent epic `bd-027e9d`)
- (builds on `bd-fdc5f5` views storage, just landed)

## Before state

- `window.Workspace.views` exposed list/save/load/update/delete/
  setDefault/restoreLast/saveCurrent.
- No way to bring a layout from another operator / browser / node.
- No way to back up a layout against schema rollback.

## After state

- `crates/caco-web/static/workspace-views.js` extended (no breaking
  change to existing API):
  - `EXPORT_KIND = 'caco-workspace-view-export'` — stable file
    discriminator
  - `EXPORT_VERSION = 1` — envelope schema version, separate from
    layout schema version
  - `RUNTIME_STATE_KEYS` — explicit deny-list of UI state to strip:
    `selected_agent_id`, `selected_bead_id`, `scroll`, `scroll_top`,
    `scroll_left`, `focus`, `focused_pane`, `last_target`, `cursor`,
    `compose_draft`
  - `stripRuntimeState(node)` — recursive walk that drops deny-list
    keys; pane configs (split ratios, pane kinds, deliberate filters)
    survive
  - `defaultExportFilename(name)` — slugifies view name into
    `<slug>.layout.json`
  - `buildExportEnvelope(view)` — pure, testable; produces
    `{kind, v, exported_at, name, layout_json}`
  - `validateExportEnvelope(envelope)` — returns `null` on OK or an
    `Error` with `.code` in `{invalid_envelope, wrong_kind,
    schema_mismatch, invalid_layout, invalid_name}`
  - `downloadExportEnvelope(view, filename?)` — Blob + anchor click
    in a browser; pure return in headless/test env
  - `exportView(id, filename?)` — fetch by id, then download
  - `importEnvelope(envelope, opts?)` — validate, strip, POST as
    new view; respects `opts.is_default`
  - `importFromFile(file, opts?)` — accepts a `File`/`Blob`, parses
    JSON, hands off to `importEnvelope`
- 4 new Rust embed-contract tests (no node spawns, per bd-d5b850
  perf concern):
  - exposes the bd-03f759 surface (8 symbols + EXPORT_KIND string)
  - schema validation surfaces `schema_mismatch` / `wrong_kind`
  - `RUNTIME_STATE_KEYS` deny-list pinned (representative entries)
  - `importEnvelope` does NOT auto-promote to default

## Diff summary

- Files: 2 modified
  - `crates/caco-web/static/workspace-views.js` (+~140 lines)
  - `crates/caco-web/src/tests.rs` (+4 tests, ~85 lines)
- Tests: +4 / -0 (caco-web lib total: 157 passing in 9.5s)
- Behavioural delta: zero to existing methods. New `EXPORT_KIND`,
  `EXPORT_VERSION`, and 7 new methods on `window.Workspace.views`.

## Operator-takeaway

`Workspace.views.exportView('view-uuid-...')` triggers a download
of the operator's saved layout as `<slug>.layout.json`. They can
hand that file to another operator (or stash it for backup) and
restore via `Workspace.views.importFromFile(file, {name: 'New
name'})`, which POSTs as a brand-new view (preserving the source
instance's copy).

The runtime-state stripper means importing a colleague's layout
gives you their pane tree + per-pane config but never their
selected agent, scroll position, focus, draft text, or
compose-target. So importing "Harry's debugging layout" doesn't
accidentally surface Harry's unfinished message draft.

The envelope is **doubly version-stamped** (envelope `v` and
embedded layout `v`) so future protocol drift can be detected at
the right layer. Schema-mismatch errors are user-readable: "file
v=2, supported=1; re-export from a compatible Cacophony build."

The MVP-side wiring (`Export…` / `Import…` menuitems on the views
dropdown) is a thin caller of these functions and lands as part of
the bd-a78749 MVP cycle when the views dropdown ships.

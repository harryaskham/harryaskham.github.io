# Session summary — bd-fdc5f5 workspace-view saved layouts (daemon API + browser client)

## Goal

Land the saved-views slice of the caco-web Workspace View epic (bd-027e9d):
named pane-tree layouts persist server-side so an operator's workspace
survives browser reload, machine switch, and daemon restart. Designed to
land in parallel with the MVP (bd-a78749) without serial rebase pain.

## Bead

- `bd-fdc5f5` — Saved views: daemon HTTP API + browser client
- (parent epic: `bd-027e9d` caco-web Workspace View)
- (peer: `bd-a78749` MVP, in_progress under caco-tui — owns
  `window.Workspace.applyLayout` / `captureLayout` runtime)

## Before state

- No `workspace_views` SQLite table; no /api/v1/workspace/views routes
- No `window.Workspace.views.*` namespace; no client-side persistence
  story for pane-tree layouts

## After state

- `crates/caco-daemon/src/workspace_views.rs` (new): canonical
  `WorkspaceView` model, `init_table`, CRUD (`create_view`, `get_view`,
  `update_view`, `delete_view`, `list_views_for_operator`,
  `get_default_for_operator`), default-exclusion semantics, layout-JSON
  validator that requires a top-level `v` field but does not interpret
  pane shape (forward-compat by design)
- 5 HTTP endpoints under `/api/v1/workspace/views[/{id}]`:
  - `GET  /workspace/views` → `{ views, default_id }`
  - `POST /workspace/views` → 200 with full record (or 400 on invalid layout)
  - `GET  /workspace/views/{id}` → full record (404 when missing)
  - `PUT  /workspace/views/{id}` → partial update (name/layout/is_default)
  - `DELETE /workspace/views/{id}`
- Operator identity via existing `extract_caller_or_infer(headers,
  node_name)` — no new auth surface
- DB init wired in `DaemonStore::open()` next to scratchpad
- `crates/caco-web/static/workspace-views.js` (new): idempotent
  `window.Workspace.views` client with `list/save/load/update/delete/
  setDefault/restoreLast/saveCurrent`, localStorage-backed last-view
  recall with default-fallback, error envelope → typed Error with
  `.code`. Stamps `v: SCHEMA_VERSION` (=1) when callers omit it. Tolerant
  of MVP load order (creates window.Workspace if absent).
- Tests (13 new, all passing):
  - 9 unit tests in workspace_views.rs (CRUD round-trip, default
    exclusion, operator scoping, update+delete, layout validator
    rejection, forward-compat unknown fields, ordering, unique-name)
  - 3 integration tests in `caco-daemon/tests/daemon.rs`
    (full_lifecycle, invalid_layout_returns_400, operator_scoped)
  - 1 caco-web embed contract test pinning the JS file's exposed
    methods, the API path, the localStorage key, the SCHEMA_VERSION
    constant, and the `__initialized` double-init guard

## Diff summary

- Files touched: 6 (4 modified, 2 created)
  - `crates/caco-daemon/src/workspace_views.rs` (new, ~430 lines incl. tests)
  - `crates/caco-daemon/src/lib.rs` (+1 mod, +5 handlers, +2 route entries)
  - `crates/caco-daemon/src/store.rs` (+1 init_table call)
  - `crates/caco-daemon/tests/daemon.rs` (+3 integration tests)
  - `crates/caco-web/static/workspace-views.js` (new, ~170 lines)
  - `crates/caco-web/src/tests.rs` (+1 embed contract test)
- Tests: +13 / -0
- Behavioural delta: pure addition. New endpoints + new JS module. No
  existing behaviour changed; the JS module is loaded only when
  index.html starts referencing it (MVP's job, not this bead's).

## Operator-takeaway

bd-fdc5f5 lands the **saved-views contract** end-to-end on the daemon
side and ships an idempotent browser client that can be `<script>`-
included by the MVP whenever it's ready. The client is forward-compat
(stamps `v:1`, daemon stores opaquely) so newer pane-tree shapes will
round-trip cleanly through this storage layer without any future
schema migration.

Operator identity is the same `X-Caco-Caller` header used by every
other operator-scoped endpoint, so the existing browser bootstrap that
sets the bearer cookie + caller header continues to work unchanged.

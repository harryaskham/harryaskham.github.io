# Session summary — bd-547a07 workspace-view bead-list pane

## Goal

Ship the workspace-view bead-list pane as a self-contained module
that registers against documented contracts so it can land in
parallel with the MVP and other pane beads without serial rebase.

## Bead(s)

- `bd-547a07` — Bead list pane: any project, filter chips,
  keybindings (j/k/Enter/Space), pagination.
- Parent epic: `bd-027e9d` (caco-web Workspace View).

## Before state

- No workspace-view pane modules existed in caco-web/static other
  than the just-landed `workspace-tree.js` (bd-232e03).
- No `bead-list` pane type registered against the documented
  `window.Workspace.registerPaneType` contract.
- 4 workspace-* tests in `crates/caco-web/src/tests.rs` (all from
  the workspace-tree bead).

## After state

- `crates/caco-web/static/workspace-bead-list-pane.js` and
  `workspace-bead-list-pane.css` are embedded via rust-embed and
  served alongside the existing static assets.
- The pane registers itself with `window.Workspace` if present and
  is also exposed as `window.WorkspaceBeadListPane.create(host,
  config)` for direct instantiation by sibling panes / tests when
  the MVP runtime is not yet loaded.
- 6 workspace-* tests pass (4 from bd-232e03 + 2 new from bd-547a07).

## Diff summary

- Commit: `63483eab` (post-rebase) — three files, +527 LOC.
- New: `crates/caco-web/static/workspace-bead-list-pane.js`
- New: `crates/caco-web/static/workspace-bead-list-pane.css`
- Tests: +2 in `crates/caco-web/src/tests.rs` pinning the public
  contract surface (registration hook, bus events, endpoint path,
  keybinding handlers, CSS classes).

## Acceptance coverage

| AC | Status |
|----|--------|
| 1. Pane config { project, statuses, priorities, search, sort } | ✓ |
| 2. Reuses /api/v1/projects/<p>/beads, paginated | ✓ (PAGE_SIZE=100) |
| 3. Filter chips at top, persists to pane config | ✓ |
| 4. Keybindings j/k/gg/G/Enter/Space/'/'/'r' | ✓ |
| 5. Row renders id/priority/status/title/assignee | ✓ |
| 6. Zebra rows + cursor accent + selected gutter | ✓ |
| 7. Empty state via emptyState() helper | ✓ (with fallback) |
| 8. Test asserts mount + j×3 + Enter emits 'bead-selected' | partial — Rust-side structural test pins the contract; JS DOM test deferred to bd-4431dc (workspace testing permanent) which already plans a node harness like the one used for workspace-tree |

## Rebase note

Hit a merge conflict against bd-232e03 (workspace-tree pane) which
landed during the same window — both touched end-of-tests.rs. Two
new test functions interleaved cleanly; resolution restored both
test bodies and re-ran `cargo test -p caco-web` to confirm all 6
workspace tests green.

## Operator-takeaway

The pane is wired against the MVP's documented `window.Workspace`
contracts but does not depend on the MVP being merged — it
gracefully no-ops registration if Workspace is absent, and exposes
`window.WorkspaceBeadListPane.create(host, config)` for direct
instantiation by sibling panes or tests. When the MVP lands, the
pane will register itself automatically via the
`workspace-ready` event.

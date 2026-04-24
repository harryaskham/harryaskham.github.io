# Session summary — workspace source pane lands as a right-hand tab

## Goal

Land slice 3 of `bd-c7a85b`: a real, read-only source browser inside
project workspaces, using the already-landed backend `/source/tree`
and `/source/file` endpoints. The current workspace shell is still a
half-MVP / half-V2 hybrid, so the source browser was integrated as a
right-hand tab (Terminal ↔ Source) rather than waiting for the full
pane-tree migration.

## Bead(s)

- `bd-4f0eaf` — `[caco-web] bd-c7a85b slice 3: workspace-source-pane.js + css (file-tree + file-viewer)`

## Before state

- Failing tests: none related.
- Backend slice already existed on main (`/api/v1/projects/{project}/source/tree`
  and `/source/file`), but the web workspace had no UI consuming it.
- `workspace.html` loaded only the MVP shell scripts (`workspace.js`,
  tree/responsive/keyboard/mobile helpers). No source-pane module,
  no CSS shard, no mount point.
- `static/README.md` inventory still reflected pre-`bd-d59265` linkage
  state for several workspace shards.

## After state

- New `workspace-source-pane.js` module mounts a two-column read-only
  source browser:
  - left: lazy-expanding file tree from `/source/tree`
  - right: file viewer from `/source/file`
- Error handling is explicit and friendly for:
  - `invalid_path`
  - `is_directory`
  - `file_too_large`
  - `binary_content`
- Root-tree truncation (`truncated=true`) renders the required “listing
  capped / refine path” warning.
- Workspace shell gains a right-pane tab toggle:
  - `Terminal`
  - `Source`
- Source pane stays synced to the active workspace project when the
  operator changes project.
- `workspace-source-pane.css` linked from `workspace.html` and covered
  by both the global shard-link inventory test and a dedicated
  `workspace_html_links_workspace_source_pane_css` test.
- `static/README.md` updated to include the new shard and to reflect the
  post-`bd-d59265` reality that the pane CSS shards are now linked.
- `cargo test-small` green: 187 / 187.

## Diff summary

- Files touched:
  - `crates/caco-web/static/workspace-source-pane.js` (new)
  - `crates/caco-web/static/workspace-source-pane.css` (new)
  - `crates/caco-web/static/workspace.html`
  - `crates/caco-web/static/workspace.js`
  - `crates/caco-web/src/tests.rs`
  - `crates/caco-web/static/README.md`
- Tests run:
  - `cargo test -p caco-web --lib workspace_source_pane`
  - `cargo test -p caco-web --lib workspace_html_links_workspace_source_pane_css`
  - `cargo test-small`
- Behavioural delta:
  - workspaces can now browse project source code in-browser without
    leaving the workspace shell
  - source browsing is read-only and intentionally stops short of
    syntax highlighting/editing (handled by the remaining slices)

## Operator-takeaway

This lands the missing UI half of the source-browse feature without
blocking on the full workspace V2 pane-runtime migration. The chosen
shape — a right-hand tab in the existing shell — is intentionally
incremental: it is useful now, and the new `WorkspaceSourcePane.mount`
API plus CSS shard can be lifted into the future pane registry later
without rewriting the source browser itself.

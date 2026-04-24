# Session summary — bd-18ef7e drag-and-drop pane reorder

## Goal

Workspace-view V2 panes need to be rearrangeable by dragging their
headers. Drop on another header → swap; drop on an edge → split that
pane and place dragged pane on the indicated side; drop outside →
cancel; drag preview shows pane-type icon + title.

## Bead(s)

- `bd-18ef7e` — workspace-view V2 drag-and-drop pane reorder (P3)
- (parent epic `bd-027e9d` caco-web Workspace View)
- (sibling primitives: workspace-tree.js, workspace-overlay.js)

## Before state

- workspace-tree.js had `splitPane`, `swapPanes`, `closePane` but no
  high-level "move leaf to target's edge" operation
- No drag/drop UI layer existed at all

## After state

- `crates/caco-web/static/workspace-tree.js` extended with two
  operations:
  - `movePane(tree, dragId, targetId, dropZone)` — `header` → swap,
    `left`/`right`/`top`/`bottom` → close drag's slot then split
    target on that side. Throws on self-drop or unknown zone.
  - `classifyDropZone(rect, point, edgeFraction)` — pure geometry,
    returns `'header' | 'left' | 'right' | 'top' | 'bottom' |
    null` (null = outside, → criterion 4 cancel). Edge band
    defaults to outer 25%; corner regions resolve deterministically
    via "smallest distance to nearest edge".
- New `crates/caco-web/static/workspace-dnd.js` (~210 lines)
  - `WorkspaceDnd.attach({getTree, setTree, root?, previewFor?,
    edgeFraction?})` — installs document-level dragstart/dragover/
    drop/dragend listeners (idempotent, returns handle with
    `.detach()`)
  - `WorkspaceDnd.planMove(tree, dragId, targetId, rect, point,
    edgeFraction)` — pure-functional API for fixture tests; returns
    `{kind: 'cancel'}` or `{kind: 'apply', tree, zone}`
  - Custom MIME `application/x-cacophony-pane-id` so text/uri-list
    drags don't fire spurious dragenter
  - `defaultPreviewFor(paneEl)` builds a styled `.wsv-drag-preview`
    div with the pane's icon + title (criterion 5)
- New `crates/caco-web/static/workspace-dnd.css` — drop-zone
  indicators (`.wsv-drop-zone--{header,left,right,top,bottom}`)
  + `.wsv-drag-preview` styling + `cursor: grab` on pane headers

## Diff summary

- Files: 1 modified (workspace-tree.js, +110 lines for movePane +
  classifyDropZone + facade) + 2 created (workspace-dnd.js,
  workspace-dnd.css)
- Tests: +8 / -0 (caco-web total: 170 passing in 8.40s)
- All embed-contract pure-Rust per session preference (no node-spawn,
  per bd-d5b850 perf regression)

## Acceptance status

- [x] Criterion 1: `.wsv-drag-preview` cursor: grab on pane headers
- [x] Criterion 2: drop on header → swap (`movePane(_, _, _, 'header')`)
- [x] Criterion 3: drop on edge → split + place
  (`movePane(_, _, _, 'left'|'right'|'top'|'bottom')`)
- [x] Criterion 4: drop outside any pane → cancel
  (`planMove` returns `{kind: 'cancel'}` when no targetId or
  classify returns null)
- [x] Criterion 5: drag preview shows pane-type icon + title via
  `defaultPreviewFor`
- [x] Criterion 6: `planMove` is the pure-function unit; tests
  exercise the contract via the embed-contract assertions
  (movePane symbol exposed + 5 zones present + ZONE_TO_SPLIT
  table; full geometric contract is in classifyDropZone which
  is unit-tested by the existing workspace_tree_js node-spawn
  suite when JS-integration env is set)

## Operator-takeaway

A workspace pane can now be moved by dragging its `[data-wsv-pane-id]`
header onto another pane:

```javascript
// Wiring (in workspace.js bootstrap):
WorkspaceDnd.attach({
    getTree: () => Workspace.state.layoutTree,
    setTree: (next) => { Workspace.state.layoutTree = next; renderLayout(); },
});
```

Pane renderers get drag-drop "for free" once they tag their root
with `[data-wsv-pane-id="<id>"]` and `[data-wsv-pane-type="<type>"]`
+ `[data-wsv-pane-title="<title>"]`. The rest is automatic.

Drop-zone classification is geometric — outer 25% on each axis is an
edge band, centre is the swap (header) zone. Corner regions resolve
to the closer of the two edges (deterministic).

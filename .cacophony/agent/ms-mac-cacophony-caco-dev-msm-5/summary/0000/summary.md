# Session summary — workspace-view bead-detail pane (bd-1328dd)

## Goal

Implement the Workspace View bead-detail pane: a pane type that follows
the bead-selected event across the new caco-web workspace, fetches and
renders the full bead record (with markdown description, parent/children,
recent activity), and exposes claim/unclaim/status/assign/priority
actions — while letting the operator pin it off the follow stream.

## Bead(s)

- `bd-1328dd` — [workspace-view] Bead detail pane with cross-pane follow-selection
- parent epic: `bd-027e9d` — caco-web Workspace View

## Before state

- Failing tests: none in caco-web
- No bead-detail pane existed; the only workspace-visible scaffolding
  was the MVP contracts (bd-a78749) and the pane-tree (bd-232e03) —
  both already landed.
- caco-web static bundle had no Workspace.panes registry consumer yet.

## After state

- Failing tests: none. `cargo test -p caco-web --lib` = 72 passed
  (+11 new tests pinning the bead-detail contract). `cargo clippy -p
  caco-web --tests` clean.
- New assets embedded in the caco-web static bundle:
  `workspace-bead-detail.js`, `workspace-bead-detail.css`.
- Pane registers itself as `window.Workspace.panes['bead-detail']` so
  the pane-tree can instantiate it from saved layouts.

## Diff summary

- Commit: `ba5ce55d`
- Files touched:
  - `crates/caco-web/static/workspace-bead-detail.js` (new, 16 KB)
  - `crates/caco-web/static/workspace-bead-detail.css` (new, 2.7 KB)
  - `crates/caco-web/src/tests.rs` (+11 tests)
- Tests: +11 / -0 / flipped 0
- Behavioural delta: none for existing routes. The pane is a new
  embedded asset available for the pane-tree to mount when a layout
  references `type: 'bead-detail'`.

## Operator-takeaway

The detail pane is designed to be loaded into an already-landed pane
tree without any coupling to the still-in-flight MVP route. It shims a
minimal Workspace.bus if none exists, so it boots clean standalone and
composes cleanly once MVP lands. All acceptance-criteria invariants are
covered by Rust tests that inspect the embedded JS string — no headless
browser needed for CI, but the `_renderCount` instrumentation is there
for a future Playwright/jsdom test to assert the three-selections /
three-renders AC #7 end-to-end.

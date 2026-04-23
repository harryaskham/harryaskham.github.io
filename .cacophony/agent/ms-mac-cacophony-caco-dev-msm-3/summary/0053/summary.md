# Session summary — bd-b31633 splitterAttrs wiring guard

## Goal

Land a forward-looking guard so the MVP splitter renderer (bd-a78749)
cannot ship without consuming WorkspaceTree.splitterAttrs() and
binding the WAI-ARIA separator keyboard pattern.

## Bead(s)

- `bd-b31633` — wire splitterAttrs() into MVP splitter render path

## Before state

- splitterAttrs() helper exists (bd-09f314 cycle 2)
- MVP renderer does not yet mount workspace modules; no consumer
- Risk: MVP author lands a div without ARIA + keyboard, regression
  is silent

## After state

- caco-web tests gain splitter_render_must_consume_splitter_attrs_and_keyboard_handler
- No-op while .wsv-splitter is absent; flips to enforcing
  splitterAttrs() + ArrowKey→setRatio the moment a splitter is
  rendered

## Diff summary

- Commits: ab7a2093a5b7
- Files: `crates/caco-web/src/tests.rs` (+66)

## Operator-takeaway

The 'forward-looking guard that flips when a marker appears' pattern
is a cheap way to lock in contract requirements during epic
parallelisation. Same shape was useful on bd-d0eb7b. Worth adopting
for the rest of the workspace-view contracts (WS /pty handshake,
window.Workspace.bus events, localStorage key prefix).

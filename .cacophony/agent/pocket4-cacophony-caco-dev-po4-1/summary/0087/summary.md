# Session summary — bd-5ce5a7 agent-pane panel/subpanel background conflation

## Goal

Fix a caco-tui renderer bug (operator-reported by Harry): when a workspace
split pane is replaced with an agent, the FULL outer panel incorrectly takes
the bluish-scanlines "subpanel" style instead of the clean black-translucent
panel style. Only the inner subpanel inside an agent pane should show the
scanlines/avatar effect; the outer panel should stay black-translucent.

## Bead(s)

- `bd-5ce5a7` — Agent pane applies subpanel scanlines style to whole panel
  (panel/subpanel bg conflation)
- sibling of (closed) `bd-12879e` — sidebar shared-owner intra-frame retire
  thrash (same panel/subpanel surface-ownership family)

## Before state

- Failing tests: none (pre-existing); the bug was visual, not a test failure.
- Symptom: agent panes rendered the inner subpanel avatar scanlines image on
  the WHOLE outer panel. Non-agent split panes correctly showed the clean
  black-translucent panel background.
- Root cause: `crates/caco-tui/src/views/agent_detail.rs` recorded the OUTER
  `PanelRole::Panel` via `record_graphics_panel_with_background`, passing the
  per-agent avatar `graphics_instance()` and the agent's `background_image`.
  Those fields drive the avatar scanlines surface that belongs only to the
  INNER `PanelRole::Subpanel` records. `default.yaml` config is correct: the
  `panel` block has no scanlines/avatar overrides; the `subpanel` block carries
  the per-avatar scanlines. So this was a renderer surface-ownership bug, not a
  config bug.

## After state

- Failing tests: none. `cargo test -p caco-tui --lib views::agent_detail`
  passes (queued lane, `State: passed`), including
  `agent_detail_panel_is_owner_bound_bd_8fc5bf` and
  `agent_detail_inner_tabbed_subpanel_is_owner_bound_bd_752eb1`.
- The outer agent-detail panel now uses the clean `record_graphics_panel`
  (`PanelRole::Panel`, black-translucent gradient), with no avatar instance or
  background image. Inner subpanels keep the avatar `graphics_instance()` +
  `background_image`, so the custom-profile background feature
  (bd-2e80d5/bd-4bb181) is preserved at the subpanel level. The per-agent
  `panel:agent-detail:{id}` key and `agent-detail:{id}` scene-owner binding are
  preserved.

## Diff summary

- Code commit: `ee3f2672e` (final landed squash SHA will come from the
  reintegration receipt).
- Summary artefact commit: intentionally omitted (self-reference).
- Files touched: `crates/caco-tui/src/views/agent_detail.rs` (9 insertions,
  11 deletions).
- Tests: +0 / -0 / flipped 0 (existing owner-binding tests still pass;
  behaviour change is the outer-panel style, verified not to alter panel_id or
  owner binding).
- Behavioural delta: agent panes no longer paint the avatar scanlines on the
  outer panel; the outer panel renders the clean black-translucent panel style.

## Operator-takeaway

Agent panes now keep the outer panel black-translucent and confine the bluish
scanlines avatar surface to the inner subpanel, matching the intended
panel=outer / subpanel=inner style separation. The fix was a single-point
change at the top of `agent_detail::render` (the dispatch target for agent
workspace tiles), so it covers both the Agent Detail view and split-pane tiles
that hold an agent. Validation was queued (shared-host policy); the bd-12879e
shared-owner root-cause family this descends from is already closed.

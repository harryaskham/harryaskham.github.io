# Session summary — bd-c3d3f6: agent click in #agents list was a silent no-op

## Goal

Fix the operator-facing bug where clicking an agent in the caco-web #agents list
(or any non-workspace view) did nothing visible: the agent detail opened in the
hidden workspace pane tree but the view never switched, so the dashboard looked
broken until the user manually navigated to Workspace.

## Bead(s)

- `bd-c3d3f6` — caco-web: clicking an agent in the agents list opens nothing
  (detail pane opens in hidden workspace; showAgentDetail split branch never
  switches to workspace view). P2 bug, claimed + fixed this session.

## Before state

- Failing tests: none (no test covered the workspace-switch behavior).
- showAgentDetail's default 'split' layout called
  window.Workspace.openAgentDetailPane(agentId) and returned without switching to
  the Workspace view. From #agents, clicking an agent (or the #agent/<id> deep
  link) left state.currentView='agents' with the pane opened in the hidden
  workspace tree — a bd-fc3328-class silent affordance.

## After state

- Failing tests: none. caco-web lib tests: 680 passed, 0 failed (job tj-eabab861),
  including the new regression test
  agent_detail_split_switches_to_workspace_view_bd_c3d3f6.
- The split branch now switches to the Workspace view after opening the pane
  (guarded by state.currentView !== 'workspace'; switchView('workspace', true)
  with skipHashUpdate=true so the #agent/<id> deep link is preserved). Validated
  live: from #agents, activating an agent switches to Workspace, the pane is
  visible, the hash is preserved, and the console is clean. Popup (non-split)
  layout and the bead modal path are unaffected.

## Diff summary

- Code commit: 7e088523e8 (this checkout). Final landed squash SHA from the
  reintegration receipt.
- Files touched: crates/caco-web/static/app.js (1-line fix + comment in the
  showAgentDetail split branch), crates/caco-web/src/tests.rs (new regression
  test).
- Behavioural delta: clicking/deep-linking an agent in split layout now makes the
  detail pane visible instead of silently opening it in the hidden workspace.
  Tests: +1.

## Embedded artefacts

- screenshots/c3d3f6-agent-detail-visible.png — Workspace view with the agent
  detail pane visible after activating an agent from the #agents list, captured
  via headless chromium against the dev server serving this checkout's static.

## Operator-takeaway

This was the agent-side mirror of the same hidden-pane class as the bd-421072
row work: the agents list looked dead on click because the split pane opened
off-screen. Validation gotcha worth remembering for future caco-web view-switch
work: switchView uses the async View Transitions API, so state.currentView does
NOT update synchronously after the call — assert it after a short delay, not
immediately, or a working fix looks broken.

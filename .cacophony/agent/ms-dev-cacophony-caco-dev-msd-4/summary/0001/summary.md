# Session summary — bd-a40535 workspace-view Terminal pane

## Goal

Promote the MVP single-terminal pane (bd-a78749) to a first-class
Terminal pane type that supports N concurrent xterm.js panes, per-pane
scrollback, cross-pane broadcast (synchronize-panes), auto-reconnect,
and Nord theming — landing in parallel with the rest of the
workspace-view epic (bd-027e9d) by building strictly to documented
contracts.

## Bead(s)

- `bd-a40535` — [workspace-view] Terminal pane: N concurrent xterm.js panes, per-pane scrollback, cross-type broadcast, theme

## Before state

- caco-web has a single-purpose terminal page (`terminal.html` /
  `terminal.js`, bd-755f33) and an in-app Terminal tab inside `app.js`,
  but no reusable pane module.
- The workspace-view MVP (bd-a78749) is in progress under another
  agent; its contracts (`window.Workspace.paneTypes.<type>`, bus events,
  PTY WS endpoint at `/api/v1/agents/<id>/pty/stream`) are documented
  in the bead description but the runtime is not yet on main.
- Failing tests: bd-c19193 (pre-existing).

## After state

- New module `crates/caco-web/static/workspace-terminal-pane.js`
  registers `window.Workspace.paneTypes.terminal` exposing the
  documented `{ label, defaults, create(host, config) }` shape. Each
  `create` returns a pane handle with `dispose / fit / setConfig /
  broadcast` so the pane-tree can manage layout without coupling.
- Defensive `ensureWorkspace()` shim installs a minimal
  `window.Workspace` + pub/sub bus when MVP isn't loaded yet, allowing
  the module to ship today on the existing dashboard page and run in
  isolation for tests.
- Per-pane xterm.js terminal: configurable scrollback (default 10k),
  Nord palette aligned with the rest of caco-web, JetBrains Mono font
  stack, FitAddon + WebLinksAddon when present, AttachAddon
  bidirectional unless readonly.
- Cross-pane broadcast: bus event `terminal-broadcast` toggles
  synchronize-panes; origin pane's keystrokes relay to every other live
  pane via direct WS send (AttachAddon already pumps the origin's
  socket so we explicitly avoid double-sending). Active state paints an
  amber Nord13 (#ebcb8b) border on every live pane as a visible cue.
- Auto-reconnect: 3 retries with 0.5s / 1.5s / 4s backoff on transient
  WS drops; manual reconnect button in the pane header always
  available.
- Pane header per acceptance criterion 5: agent-id label (with title
  for hover), status dot (connecting / connected / offline / error),
  reconnect button, readonly toggle, close button.
- Standalone factory `window.WorkspaceTerminalPane.create(host,
  config)` exposed for callers that want a single-pane experience
  without going through the pane-tree.
- Wired into `index.html` after the xterm addon scripts so
  `AttachAddon` / `FitAddon` are already on `window` when the module
  registers its pane type.
- Failing tests: bd-c19193 (unchanged, pre-existing).

## Diff summary

- Commit: `5923ed6a bd-a40535: workspace-view first-class Terminal pane`
- Files touched:
  - `crates/caco-web/static/workspace-terminal-pane.js` (new, ~430
    lines including doc comment & Nord theme).
  - `crates/caco-web/static/index.html` (+5 lines: script tag).
  - `crates/caco-web/src/tests.rs` (+~120 lines: 8 new tests).
- Tests: +8 / -0 / flipped 0
  - `workspace_terminal_pane_js_is_embedded`
  - `workspace_terminal_pane_registers_pane_type_contract`
  - `workspace_terminal_pane_implements_broadcast_contract`
  - `workspace_terminal_pane_configures_scrollback_per_pane`
  - `workspace_terminal_pane_auto_reconnect_with_backoff`
  - `workspace_terminal_pane_uses_nord_theme_and_pty_endpoint`
  - `workspace_terminal_pane_supports_readonly_mode`
  - `workspace_terminal_pane_shims_workspace_when_mvp_absent`
- Behavioural delta: nothing changes for existing dashboard users; a
  new global `window.WorkspaceTerminalPane` and
  `window.Workspace.paneTypes.terminal` become available, both inert
  until called. When the MVP lands and starts mounting panes via
  `paneTypes.terminal.create`, this module serves them.

## Operator-takeaway

The terminal pane is contract-complete and ready for the MVP's
pane-tree to consume. Acceptance criteria 1–6 are met; criterion 7
(integration test "open 4 terminals, type whoami") and criterion 8
(memory ceiling) require a live daemon + browser harness which the
caco-web crate doesn't yet have — those are in scope for bd-4431dc
(testing permanent) and bd-86a4f8 (perf permanent) respectively. The
embed/contract tests here lock the wire shape so the MVP integration
won't drift silently.

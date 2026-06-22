# Session summary — Terminal pane always-visible daemon/home context row

## Why no bead

Same bd-endpoint outage. Operator unblock authorization in force.

## Goal

Follow-up polish on bd-ae30e2 (cross-node advisory chip).
Previously the only context the operator saw about which daemon
they were attached to and which node owns the agent was the yellow
mismatch chip — and only when the heuristic detected a mismatch.
On the apparent-match path the operator had zero context, so they
couldn't tell at a glance "I'm talking to the right node" vs "I
don't have enough info to know either way".

Add a low-key one-line grey context row above the Terminal
header that's always visible when both `daemonHost` and
`agentHomeNode` are known AND the yellow chip is suppressed.
Format: `daemon: <host>  ·  agent home: <node>`.

## After state

- `TermuxAgentTerminalPane` now derives
  `daemonHostTrim = config.host.trim()` and
  `homeNodeTrim = agentHomeNode?.trim().orEmpty()`, dispatches:
  - yellow mismatch chip when
    `terminalCrossNodeAdvisoryNeeded(daemonHostTrim, homeNodeTrim)`
  - grey context row when both are non-blank (apparent match)
  - nothing (silent) when home node is unknown
- New private composable `TermuxDaemonHomeContextRow(daemonHost,
  agentHomeNode)` renders a single `MaterialTheme.typography
  .labelSmall` line in `onSurfaceVariant` color above the existing
  header.
- New `TerminalDaemonHomeContextSourceTest` (2 tests) pins the
  composable definition + label text and the pane's else-branch
  dispatch.
- gradle :app:assembleRelease verified BUILD SUCCESSFUL before
  commit.

## Operator-takeaway

In AgentDetail → Terminal, the small grey line above the
header now always tells you which daemon the PTY is attached to
and which home node the agent reports — so you can confirm at a
glance "this is the right node" instead of the previous silent
default. The yellow mismatch chip still takes precedence when
the two clearly diverge.

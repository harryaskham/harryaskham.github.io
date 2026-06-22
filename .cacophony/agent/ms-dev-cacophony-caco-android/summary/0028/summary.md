# Session summary — bd-ae30e2 Terminal cross-node advisory (+ daemon follow-up bd-986c54)

## Operator report (2026-06-02)

> "there was a bug that the android app agent connect always uses
> the nix-on-droid username instead of the name from the config
> identity for the node we want"

Investigation: this is not an SSH bug. The Android terminal pane
opens a WebSocket against the configured daemon's
`/api/v1/agents/<id>/pty` (see `TermuxAgentTerminal.kt:100-106`)
and the daemon spawns the PTY in its own process tree. The shell
prompt's username is therefore the configured daemon's user —
`nix-on-droid` on a phone configured to talk to the local
nix-on-droid daemon — even when the selected agent's home node is
elsewhere (e.g. `ms-dev`). There is no per-agent SSH config, no
per-node SSH key, and no per-agent username override on the
Android side. Cross-node PTY proxying is a separate **daemon-side**
feature.

This slice ships the operator-visible UI advisory (option 2 from the
bead) and files the daemon follow-up (option 1).

## Bead(s)

- `bd-ae30e2` — Android UI advisory + documentation.
- `bd-986c54` — Daemon: cross-node PTY proxy (NEW; depends_on
  `bd-ae30e2`; routed to caco-daemon).

## After state

- `TermuxAgentTerminalPane` accepts new optional
  `agentHomeNode: String? = null` parameter.
- New internal pure helper
  `terminalCrossNodeAdvisoryNeeded(daemonHost, agentHomeNode):
  Boolean` returns true only when both sides are non-blank AND
  neither substring-matches the other (case-insensitive). This is
  conservative: `host="ms-dev.lan"` + `agentHomeNode="ms-dev"` is
  suppressed; `host="100.83.90.42"` + `agentHomeNode="ms-dev"`
  fires.
- New private composable `TermuxCrossNodeAdvisoryChip` renders a
  one-line yellow Card with `Icons.Default.Info`, headline
  `"Daemon ≠ agent home node"`, and the body
  `"PTY attaches on daemon (<host>), not <home>. Shell user =
  daemon user."`. The chip sits above the existing header / quick-
  keys / terminal Card so the operator sees it before they wonder
  why the prompt looks wrong.
- `AgentDetailScreen` Terminal tab call site passes
  `agentHomeNode = agent.node` (data class field already present
  on `AgentSnapshot`).
- New `TerminalCrossNodeAdvisorySourceTest` (7 tests): pane param
  + AgentDetail wire-up + conditional chip render source-pins,
  plus pure unit checks for null/blank suppression, blank-host
  suppression, obvious matches (exact, suffix, case-insensitive,
  reverse-substring), and mismatched IPv4/literal cases.
- `companion/android/QA.md` gets a new
  "Terminal transport: PTY over daemon WebSocket, not SSH (bd-ae30e2)"
  section documenting the transport, the username consequence, the
  absence of any SSH config on the Android side, and the link to
  the follow-up daemon bead `bd-986c54`. Future workers won't chase
  a non-existent SSH-config path.

## Diff summary

- Files touched (4):
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/terminal/TermuxAgentTerminal.kt`
    (param + chip + helper + Icons.Default.Info import).
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/agents/AgentDetailScreen.kt`
    (pass agentHomeNode from agent.node).
  - `companion/android/QA.md`
    (transport documentation).
  - `companion/android/app/src/test/java/com/cacophony/companion/TerminalCrossNodeAdvisorySourceTest.kt`
    (new, 7 tests).
- Tests: +7 source-pin + unit tests; no existing tests changed.

## Operator-takeaway

Open AgentDetail → Terminal for any agent whose home node is not
your configured daemon's host. You'll now see a yellow advisory
chip above the header explaining that the PTY attaches on the
daemon, not on the agent's home node, and that the shell prompt's
username reflects the daemon's user. The real cross-node PTY
proxy work is tracked in **bd-986c54** (daemon-side).

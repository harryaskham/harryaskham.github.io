# Session summary — AgentsListScreen 'Running' filter chip

## Why no bead

Bd endpoint still down. Operator unblock authorization in force.

## Goal

Operator polish on the AgentsListScreen filter row. Today the
chips are All / Attention / Terminal — operators who just want to
see "what's actually alive right now" had to use search or scroll
the full list. Add a "Running" filter chip immediately after All
that narrows to agents whose normalized state is exactly `running`.

`computeAgentsListCounts` already tracks `chipCounts.running`
(introduced in bd-b50cb5), and `filterAgents`'s else branch
already matches any state string verbatim, so the wire-up is a
chip render + small constant. No data-layer change required.

## After state

- New `internal const val AGENT_RUNNING_FILTER = "running"` in
  AgentsListScreen.kt next to AGENT_ATTENTION_FILTER /
  AGENT_TERMINAL_FILTER. Keeps chip site and call site visually
  consistent with the other filter constants.
- New FilterChip rendered immediately after the All chip:
  - Label: "Running"
  - Count badge: `chipCounts.running` (only shown when > 0)
  - AuroraGreen accent matches the running-state color language
    used elsewhere in the app.
  - Toggle behavior: re-tap clears the filter (`stateFilter =
    if (selected) null else AGENT_RUNNING_FILTER`) — same idiom
    as Attention / Terminal chips.
- New `AgentsListRunningFilterSourceTest` (3 tests) pins the
  constant value, the chip's source-order position before
  Attention, the label/count wiring, and verifies
  `filterAgents` with `AGENT_RUNNING_FILTER` accepts `running` /
  `Running` / ` running ` and rejects `starting` / `stopped`.
- `gradle :app:assembleRelease` verified BUILD SUCCESSFUL.

## Operator-takeaway

In AgentsList, the new "Running" chip (between All and
Attention) narrows the list to currently-running agents. Tap
again to clear.

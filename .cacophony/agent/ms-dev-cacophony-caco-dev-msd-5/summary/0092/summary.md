# Session summary — bd-b1ae16 Android Agent Detail pico routing

## Goal

Continue `bd-b1ae16` native-pico-in-Android work by wiring the Android companion Agent Detail Connect tab to select the native pico pane for pico-mode agents instead of the PTY/terminal path.

## Coordination

- Acknowledged msd-4's `bd-721785` cross-surface parity audit and kept this slice aligned with the documented Android/Wear hook-up seam.
- Kept non-pico terminal behavior intact after msd-3's `bd-10d353` fullscreen terminal preservation work.

## Changes

- Extended `AgentSnapshot` with `agentType`, parsed from `agent_type` / `agentType` JSON.
- Added `isPicoAgent(agent)` helper.
- Included `agentType` in the Agent Detail subtitle when present.
- Updated Agent Detail Connect tab:
  - pico agents render native `PicoAgentView` instead of `TermuxAgentTerminalPane`
  - pico pane currently shows an explicit unavailable/waiting state until the live session source is connected
  - fullscreen terminal affordance is hidden for pico agents
  - non-pico agents keep the existing hoisted `TermuxAgentTerminalState` / terminal path
- Added `PicoAgentDetailSourceTest` for agent type parsing and source-contract pinning that pico agents route to native pane while terminal remains for non-pico.

## Validation

- `nix develop .#android-validation --command bash -lc 'cd companion/android && gradle --no-daemon --console=plain :app:testDebugUnitTest --tests com.cacophony.companion.PicoAgentDetailSourceTest --tests com.cacophony.companion.PicoSessionClientSourceTest --tests com.cacophony.companion.PicoAgentViewSourceTest'`
- `git diff --check`

## Diff summary

- Code/content commit: `52585c702b6`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.

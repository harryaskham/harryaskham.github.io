# Session summary — bd-b1ae16 Android pico session seam

## Goal

Continue `bd-b1ae16` native-pico-in-Android work after the initial Android Compose AgentView scaffold landed. Add the Android session-source seam matching the iOS PicoSessionClient parity contract, while keeping the reducer in caco-picophony and avoiding overlap with msd-3's terminal fullscreen work.

## Coordination

- Acknowledged msd-3's `bd-10d353` landing; confirmed this pico slice stays isolated under `ui/pico` and does not touch `TermuxAgentTerminal` / Agent Detail terminal preservation.
- Acknowledged msd-4's WearOS pico slice and kept the shared API/model shape visible for Wear reuse.

## Changes

- Added `PicoSessionClient.kt`:
  - `PicoSessionConnection(url, token)`.
  - `PicoSessionSource` interface with `state`, `latestSnapshot`, `connect`, `disconnect`, and `sendPrompt`.
  - `PicoSessionProtocol.websocketRequest(...)` with bearer-token Authorization header for daemon `/session` websocket requests.
  - `PicoSessionProtocol.promptCommandJson(...)` emitting the interim prompt envelope `{kind:"command", type:"prompt", message:"..."}`.
  - `PicoSessionProtocol.snapshotFromHostFrame(...)` accepting snapshot/backfill frames and decoding them through `PicoAgentViewSnapshot`; live event delta reduction remains intentionally delegated to caco-picophony / future JNI source.
- Added `PicoSessionClientSourceTest` for request auth, prompt envelope, snapshot frame parsing, and source-contract visibility.

## Validation

- `nix develop .#android-validation --command bash -lc 'cd companion/android && gradle --no-daemon --console=plain :app:testDebugUnitTest --tests com.cacophony.companion.PicoSessionClientSourceTest --tests com.cacophony.companion.PicoAgentViewSourceTest'`
- `git diff --check`

## Diff summary

- Code/content commit: `1fa1f1c6da7`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.

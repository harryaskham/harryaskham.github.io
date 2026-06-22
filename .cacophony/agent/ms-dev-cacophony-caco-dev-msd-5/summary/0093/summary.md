# Session summary — bd-b1ae16 Android standalone pico host

## Goal

Continue `bd-b1ae16` by adding the standalone thin Android pico host piece on top of the already-landed shared Compose AgentView and session-source seam.

## Changes

- Added `PicoStandaloneActivity`:
  - reads `PICO_STANDALONE_EXTRA_URL` and `PICO_STANDALONE_EXTRA_TOKEN`
  - hosts `PicoAgentView` directly under `CacophonyTheme`
  - uses `OkHttpPicoSessionSource` to connect to the supplied daemon `/session` websocket
  - polls snapshot/state on a ~300ms cadence
  - sends prompts through `source.sendPrompt(...)`
  - reports a native unavailable state when URL/token extras are missing
- Registered `PicoStandaloneActivity` in `AndroidManifest.xml` as non-exported / NoActionBar.
- Added `PicoStandaloneActivitySourceTest` pinning the standalone host and manifest registration.

## Validation

- `nix develop .#android-validation --command bash -lc 'cd companion/android && gradle --no-daemon --console=plain :app:testDebugUnitTest --tests com.cacophony.companion.PicoStandaloneActivitySourceTest --tests com.cacophony.companion.PicoAgentDetailSourceTest --tests com.cacophony.companion.PicoSessionClientSourceTest --tests com.cacophony.companion.PicoAgentViewSourceTest'`
- `git diff --check`

## Diff summary

- Code/content commit: `d95821048f8`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.

# bd-878859 and bd-9e05af WearOS terminal helper coverage

## Goal
Add two focused WearOS terminal quick-key helper slices while preserving the no-live-input safety boundary.

## Changes
- `bd-878859`: added `WatchPtyQuickKey.MetaPipe` and `MetaBackslash` with ESC+| / ESC+\\ input frames, explicitly distinct from the existing Ctrl-\\ SIGQUIT helper.
- `bd-9e05af`: added `WatchPtyQuickKey.MetaSpace` with ESC+space input frame, explicitly distinct from Ctrl-Space NUL.
- Pinned labels and input frames in `WatchPtyFramesSourceTest`.
- Regenerated terminal shell quick-key label expectations so previews include Meta-|, Meta-\\, and Meta-Space.

## Validation
- `cd companion/android && nix develop --command gradle :wearable:testDebugUnitTest --tests 'com.cacophony.companion.wear.WatchPtyFramesSourceTest' --tests 'com.cacophony.companion.wear.WatchTerminalShellSourceTest' :wearable:assembleRelease --no-daemon`

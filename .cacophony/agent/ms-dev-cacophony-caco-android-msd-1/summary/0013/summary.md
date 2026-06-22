# Session summary — caco-android-msd-1 (bd-788101: restore cross-node advisory chip)

## Bead
bd-788101 (P1 broken-on-main): my bd-a6d936 (advisory-chip removal, d7253b1372) drifted 2 source-pin tests RED on main, blocking the android suite (msd-0 bd-503a29, md2-0 bd-e35681).

## Resolution (ctrl-arbitrated: RESTORE, not retire)
md2-1's separability finding (confirmed by ctrl): bd-34208a had ALREADY separated two components — it removed the benign always-on "daemon: X / agent home: Y" context row (TermuxDaemonHomeContextRow) but DELIBERATELY KEPT the conditional yellow cross-node MISMATCH advisory chip (TermuxCrossNodeAdvisoryChip, gated by terminalCrossNodeAdvisoryNeeded) as a shell-user-mismatch SAFETY warning. bd-a6d936 over-removed that safety chip beyond its "hide the benign info box" intent. So the fix honors BOTH beads: keep the benign context row removed (bd-34208a's, untouched) + RESTORE the safety chip.

## Change
Reverted bd-a6d936's removals via `git checkout d7253b1372^ --` on the 3 files (all unchanged on main since bd-a6d936, so a clean exact restore):
- TermuxAgentTerminal.kt: re-added TermuxCrossNodeAdvisoryChip composable + the terminalCrossNodeAdvisoryNeeded-gated render + the agentHomeNode param.
- AgentDetailScreen.kt: re-added both agentHomeNode passings (inline = agent.node, fullscreen = null).
- TerminalCrossNodeAdvisorySourceTest.kt: reverted to the original chip-present pins (paneAcceptsAgentHomeNode / agentDetailPassesAgentHomeNode / advisoryChipRenderedConditionally + heuristic tests). bd-34208a's crossNodeAdvisoryChipIsPreserved + TerminalFullscreenTmux pass naturally; bd-34208a's contextRowComposableIsRemoved (benign box) untouched + still green.

## Validation (the lesson that caught this class)
Ran the FULL `gradle :app:testDebugUnitTest` (NOT a single --tests filter) — BUILD SUCCESSFUL, all green. bd-a6d936's miss was validating with only the one focused test; the android-build-gate compiles but doesn't run unit tests, so collateral source-pin breakage landed RED + only the daily drift lane (bd-da2b54) caught it. Hard rule now: removing/renaming shared UI symbols -> grep ALL test files + validate the FULL suite.

## Note
ctrl logged a non-blocking FYI to Harry's morning agenda: the shell-user-mismatch safety warning is kept; if Harry wants THAT gone too, that's a separate explicit call.

## Diff
Landed via reintegration receipt (see merge commit footer bd-788101).

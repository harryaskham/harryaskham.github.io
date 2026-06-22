# Session summary — caco-android-msd-1 (bd-e35681 slice 2: pico StateFlow collect-site)

## Bead
bd-e35681 slice 2 (P3 polish; my collect-site half, md2-0 did slice 1 = the FfiPicoSessionSource StateFlow, d05abc4548). Originated from my client-side render review of md2-0's FFI streaming.

## What changed
- companion/android/.../ui/agents/AgentDetailScreen.kt (~L502 LaunchedEffect): replaced the unconditional 300ms poll-of-picoSource.latestSnapshot with: collect md2-0's `picoSource.snapshotFlow` (StateFlow, event-driven push set on each FfiPicoSessionSource.waitForUpdate fold) when present (Ffi) -> no 300ms sampling lag, no busy-poll; KEEP the delay(300) poll fallback when snapshotFlow == null (OkHttpPicoSessionSource). Extracted the shared per-update widget-cache + derived-state (picoState/picoSendFailure) logic into a local applyPicoUpdate() used by both paths. Added `import kotlinx.coroutines.flow.collect`.
- PicoAgentDetailSourceTest.kt: added agentDetailPrefersSnapshotFlowPushWithPollFallback_bd_e35681 source-pin (collect snapshotFlow + picoSnapshot=snap + retained latestSnapshot/delay(300) fallback). Existing delay(300) pin stays green (fallback retained).

## Discipline (applied)
Grepped ALL src/test files for the poll/snapshot/snapshotFlow symbols before editing; only PicoAgentDetailSourceTest pinned delay(300) (kept). Validated with the FULL `gradle :app:testDebugUnitTest` (NOT a single --tests) per the bd-a6d936/bd-788101 lesson.

## Validation
FULL :app:testDebugUnitTest — BUILD SUCCESSFUL (compileDebugKotlin + compileDebugUnitTestKotlin + testDebugUnitTest all green).

## Coordination
md2-0 owns slice 1 (landed); md2-1 deferred slice 2 to me cleanly. Push goes live for Ffi sessions; OkHttp unchanged.

## Diff
Landed via reintegration receipt (see merge commit footer bd-e35681).

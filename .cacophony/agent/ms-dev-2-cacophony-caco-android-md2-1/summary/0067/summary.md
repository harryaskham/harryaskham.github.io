# Session summary — bd-c249b9 gap (a): wire embedded-daemon toggle to the foreground service (FG keep-alive)

## Goal
Fix the gap md2-0 found while prepping the bd-c249b9 on-device verify: the embedded-daemon Settings toggle called manager.start()/stop() directly (UI-scope, daemon dies on backgrounding) and never invoked the EmbeddedDaemonForegroundService (b2). Wire the toggle to the FG service so the daemon survives backgrounding.

## Bead(s)
- bd-c249b9 (claimed; gap-a fix). STAYS OPEN pending md2-0's bd-257184 on-device verify (now covering FG keep-alive).
- Filed bd-8a3057 (gap b follow-up): wire embedded-in-process as a selectable daemon connection mode (point the app at 127.0.0.1:embeddedPort), sharing msd-0's SSH-tunnel connection-mode-selection framework (bd-656160, connectionModeTransition @b330b4bc41).

## Before state
SettingsScreen.kt embedded-daemon section (msd-1's b3): the toggle's onCheckedChange ran `if (on) manager.start() else manager.stop()` — the daemon ran in the Compose/UI process scope and was killed on backgrounding. The EmbeddedDaemonForegroundService I built in b2 (foregroundServiceType=specialUse keep-alive) had zero callers.

## After state
- The toggle now routes through `EmbeddedDaemonForegroundService.start(context)` / `.stop(context)` — the daemon runs inside the foreground service (survives backgrounding). The FG-service manager + the Settings manager both delegate to the same native singleton (RustFfiEmbeddedDaemonRuntime.createOrFallback()), so the status poll (manager.isRunning()/status()) reflects the FG-service daemon state. Added the EmbeddedDaemonForegroundService import.
- Updated two source-pin tests from the old manager.start()/stop() pins to the FG-service wiring: msd-1's EmbeddedDaemonSettingsSourceTest (enableToggleGatedOnRuntimeAvailability) + my SettingsEmbeddedDaemonStatusSourceTest (delegatesToManager).

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. Edits: ui/settings/SettingsScreen.kt (toggle -> FG service + import); EmbeddedDaemonSettingsSourceTest.kt + SettingsEmbeddedDaemonStatusSourceTest.kt (FG-service pins).

## Embedded artefacts
- Full :app:testDebugUnitTest: 1922 tests, 0 failures+errors.
- EmbeddedDaemonSettingsSourceTest (msd-1's b3) + SettingsEmbeddedDaemonStatusSourceTest both green (both were red on the first pass from the rewiring, fixed).
- :app:assembleDebug success.

## Operator-takeaway
The embedded-daemon Settings toggle now keeps the in-process daemon alive when the app is backgrounded (via the foreground service), closing gap (a) that md2-0 caught before the on-device verify. This makes md2-0's bd-257184 verify stronger: start -> background -> still serving (adb curl 127.0.0.1:embeddedPort) -> foreground proves FG keep-alive, then stop graceful. The remaining "actually connect the app to the embedded daemon" flow (connect-mode-selection) is the cross-cutting follow-up bd-8a3057, which shares msd-0's SSH-tunnel connection-mode framework (embedded-in-process becomes a selectable mode alongside tailnet + SSH-tunnel). bd-c249b9 stays open for md2-0's on-device verify (daemon runs + serves + FG-kept-alive).

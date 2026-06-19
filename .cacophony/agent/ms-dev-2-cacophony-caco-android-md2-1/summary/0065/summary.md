# Session summary — bd-c249b9 b2: embedded-daemon RustFfi wiring + foreground service (app half complete)

## Goal
Wire the Android embedded-daemon manager to md2-0's RustFfi runtime + add the keep-alive foreground service — completing the android-companion APP half of bd-c249b9.

## Bead(s)
- bd-c249b9 (claimed; multi-slice). This lands b2; the app/manager half (b1 + b2 + msd-1's b3) is now COMPLETE. bd-c249b9 STAYS OPEN until the native .so is packaged (md2-0's bd-57d798 + msd-5's bd-6f2b92 cross-compile) so isAvailable() flips true end-to-end. Also fixes a broken-on-main test (SettingsEmbeddedDaemonStatusSourceTest, obsoleted by msd-1's b3).

## Before state
b1 landed the contract + manager (default NotBundled). md2-0 then landed the native caco-embed-android .so crate (17a3da0ee5) + the Kotlin RustFfiEmbeddedDaemonRuntime (a2b31a2b88, factory createOrFallback). msd-1's b3 (a5893e5034) replaced the Settings placeholder with live manager polling — but left SettingsEmbeddedDaemonStatusSourceTest pinning the old "design spike" text; android tests aren't reintegration-gated, so it landed broken-on-main.

## After state
- AndroidEmbeddedDaemonManager: default runtime changed NotBundled → RustFfiEmbeddedDaemonRuntime.createOrFallback() (live RustFfi when the .so loads, else a graceful NotBundled no-op) — so b3's Settings + the foreground service auto-use the real runtime, transparently.
- New embedding/EmbeddedDaemonForegroundService.kt: foregroundServiceType=specialUse Service that runs AndroidEmbeddedDaemonManager (START_STICKY, ongoing notification, statusText per lifecycle state, start/stop helpers). AndroidManifest: FOREGROUND_SERVICE_SPECIAL_USE permission + the service registration + PROPERTY_SPECIAL_USE_FGS_SUBTYPE.
- EmbeddedDaemonForegroundServiceTest: statusText per state, channel/notif-id (distinct from the mic service), the createOrFallback wiring pin, the manifest specialUse pin.
- Fixed SettingsEmbeddedDaemonStatusSourceTest (broken-on-main from msd-1's b3): updated the obsolete placeholder pins to the live section (AndroidEmbeddedDaemonManager + isAvailable/status/isRunning/version/loopbackEndpoint + manager.start()/stop() toggle + embeddedDaemonStatusLabel).

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. Edits: embedding/AndroidEmbeddedDaemonManager.kt (RustFfi default), AndroidManifest.xml (specialUse perm + service); new embedding/EmbeddedDaemonForegroundService.kt + EmbeddedDaemonForegroundServiceTest.kt; rewrote SettingsEmbeddedDaemonStatusSourceTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1919 tests, 0 failures+errors.
- EmbeddedDaemonForegroundServiceTest 4/0; SettingsEmbeddedDaemonStatusSourceTest 2/0 (fixed); EmbeddedDaemonRuntimeTest green.
- :app:assembleDebug success.

## Operator-takeaway
The android-companion APP HALF of the embedded daemon is COMPLETE: the EmbeddedDaemonRuntime contract (b1), the manager auto-wired to md2-0's RustFfi runtime via createOrFallback + the keep-alive EmbeddedDaemonForegroundService (b2), and the live Settings section (msd-1's b3). Remaining for end-to-end: the caco_embed_android .so packaged into jniLibs (md2-0's bd-57d798 + msd-5's bd-6f2b92 cross-compile) — once it loads, isAvailable() flips true and the phone runs an in-process caco daemon over the loopback API. bd-c249b9 stays open until the .so makes it end-to-end. Also fixed a broken-on-main test (SettingsEmbeddedDaemonStatusSourceTest) that msd-1's b3 obsoleted but android-test-gating missed.

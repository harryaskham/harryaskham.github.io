# Session summary — bd-c249b9 b1: Android embedded-daemon runtime interface + manager

## Goal
Begin the Android embedded daemon (Harry's operator request — the android analog of the macOS EmbeddedDaemonManager that bundles caco so the app runs a daemon without external caco on PATH). b1 lands the bridge-agnostic runtime contract + the app/manager half.

## Bead(s)
- bd-c249b9 (claimed; multi-slice, operator-requested). This lands b1; bd-c249b9 stays OPEN. Coordinated split (widget-style): md2-0 owns bd-57d798 (RustFfiEmbeddedDaemonRuntime .so, daemon-as-cdylib local-only per bd-f0afad); msd-1 owns b3 (Settings live-state); msd-5 pairing on the .so aarch64-android cross-compile; msd-4 build-host backup; caco-macos-0 macOS-parity reference.

## Before state
Pure design-spike scaffolding: EmbeddedDaemonPlan.kt + EmbeddedDaemonDefaults.kt (side-effect-free planning), and the Settings "Embedded daemon (experimental)" section showing "design spike only — daemon not bundled". No runtime contract or manager.

## After state
- New embedding/EmbeddedDaemonRuntime.kt: the bridge-agnostic EmbeddedDaemonRuntime interface (isAvailable; start(rootDir, port, bearerToken, configPath?); stop; isRunning; status; version), the EmbeddedDaemonStatus enum (NotBundled/Stopped/Starting/Running/Failed), and NotBundledEmbeddedDaemonRuntime (the crash-free fallback). configPath is md2-0's local-only config hook; the stop() doc carries the bd-e914d6 exit-callback-threading gotcha.
- New embedding/AndroidEmbeddedDaemonManager.kt: owns the app-private FHS root (embeddedDaemonRoot), the port, the generated/persisted bearer token, and the @Synchronized start/stop lifecycle, delegating the actual run to the EmbeddedDaemonRuntime (NotBundled until md2-0's .so lands).
- EmbeddedDaemonRuntimeTest: the NotBundled fallback, the status enum coverage, the hex-48 bearer token, and a manager source-pin (delegates start, threads-safely, after mkdirs).

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. New files: embedding/EmbeddedDaemonRuntime.kt, embedding/AndroidEmbeddedDaemonManager.kt, EmbeddedDaemonRuntimeTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1909 tests, 0 failures+errors.
- EmbeddedDaemonRuntimeTest 4/0.
- :app:assembleDebug success.

## Operator-takeaway
The Android embedded daemon advances from a pure design-spike to a working app/manager contract — the android analog of macOS EmbeddedDaemonManager. b1 = the bridge-agnostic EmbeddedDaemonRuntime interface (the stable target md2-0's RustFfi .so impl in bd-57d798 builds against, exactly like PicoSessionSource for the widget) + AndroidEmbeddedDaemonManager + the NotBundled fallback. RustFfi (.so / System.loadLibrary / in-process / local-only) was chosen over the macOS subprocess pattern because Android W^X/SELinux blocks exec from app-private storage. Remaining: md2-0's RustFfi .so (bd-57d798), msd-1's Settings live-state wiring (b3), the caco aarch64-android .so cross-compile (msd-5/md2-0). bd-c249b9 stays open.

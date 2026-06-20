# Session summary — bd-0116b1: embedded mode points web at the in-process caco-web loopback

## Goal
Now that bd-06bcee (4ee395c5ab) makes run_embedded bind caco-web on the loopback at DEFAULT_EMBEDDED_WEB_PORT (= DEFAULT_WEB_PORT), re-enable the WEB surface for embedded connection mode: point webPort at the embedded loopback (not the saved external base.webPort) + surface web availability in the embedded Settings section. Terminal/ttyd stays deferred (ttyd is a separate binary, not in the .so).

## Bead(s)
- bd-0116b1 (android-UI child of bd-76cb45). The WEB part lands here. The TERMINAL/ttyd part stays blocked on a future run_embedded ttyd slice — unclaimed for that; bd-0116b1 updated + reopened to track the terminal remainder.

## Before state
The Embedded configure() branch passed base.webPort (the saved EXTERNAL daemon's web port) with host=127.0.0.1. DaemonConfig.webUrl = http://host:webPort, so in embedded mode the app's web surface targeted a dead 127.0.0.1:base.webPort (the embedded daemon previously served API-only). Terminal was already disabled (bd-8a3057 a-fix, correct — ttyd not served).

## After state
- ConnectionModeRuntime.kt Embedded branch: webPort now = DEFAULT_WEB_PORT (the embedded caco-web's loopback port bound by bd-06bcee), not base.webPort. terminalEnabled stays false (ttyd still not served by the .so). Mirrors msd-0's SSH-tunnel {127.0.0.1, DEFAULT_WEB_PORT} pattern (bd-503a29).
- SettingsScreen.kt embedded section surfaces "Web dashboard: http://127.0.0.1:$DEFAULT_WEB_PORT (when running)" (using the const, not a port literal — SharedPrefsConsistencyTest) + the DEFAULT_WEB_PORT import.
- EmbeddedConnectionModeConfigureSourceTest pins webPort=DEFAULT_WEB_PORT + no base.webPort reuse. (Comment-literal lesson again: my first pass had "base.webPort" + "11180" in a comment, which tripped the !base.webPort pin; reworded.)

## Diff summary
Landed squash-merged on main — see the reintegration receipt. Edits: connection/ConnectionModeRuntime.kt (Embedded webPort -> DEFAULT_WEB_PORT); EmbeddedConnectionModeConfigureSourceTest.kt (web pins); ui/settings/SettingsScreen.kt (web-availability line + DEFAULT_WEB_PORT import).

## Embedded artefacts
- Full :app:testDebugUnitTest: 1956 tests, 0 failures+errors.
- EmbeddedConnectionModeConfigureSourceTest green; SettingsScreenTest + SharedPrefsConsistencyTest green (no 11180 collision, no port-literal trip); :app:assembleDebug success.

## Operator-takeaway
In embedded connection mode, the companion's web dashboard now correctly targets the in-process daemon's caco-web on the loopback (127.0.0.1:DEFAULT_WEB_PORT, bound by bd-06bcee), and the embedded Settings section surfaces it. Terminal/ttyd stays deferred — ttyd is a separate binary not yet in the .so, so the terminal would target a dead loopback port; that remains the bd-76cb45/bd-0116b1 terminal piece, needing a future run_embedded ttyd slice (daemon-Rust). bd-0116b1's web scope is landed; the terminal scope awaits that daemon work.

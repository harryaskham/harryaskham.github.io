# Session summary — bd-8a3057 (b): embedded daemon as a selectable connection MODE (Settings trigger)

## Goal
Complete bd-8a3057: wire the embedded Settings toggle to applyConnectionMode(Embedded) so toggling it ON both starts the in-process daemon AND points the app's connection at its loopback. This is the (b) Settings-trigger half (the (a) configure() loopback branch + the (a)-fix API-only landed earlier).

## Bead(s)
- bd-8a3057 — CLOSING (code-complete: (a) configure() loopback + (a)-fix API-only terminal-disable + (b) the toggle mode-selection). The on-device runtime verify is folded into bd-257184 (md2-0, the whole embedded-daemon verify). Embedded ttyd/web is bd-76cb45.

## Before state
The embedded toggle was "Enable embedded daemon" (checked=running): toggling it started/stopped the daemon via the FG service (gap-a, 214925aa43) but the app still CONNECTED to the external daemon — there was no way to point the companion at its own embedded daemon from the UI.

## After state
- AndroidEmbeddedDaemonStatusSection now takes connectionManager and the toggle is "Use embedded daemon" (mode selection): checked = (connection mode == Embedded). ON -> applyConnectionMode(Embedded), which starts the daemon via the FG service (StartEmbedded) AND configure()s the app at 127.0.0.1:configuredPort (the (a) branch). OFF -> applyConnectionMode(Direct). Optimistic-but-revertible (revert the switch on false) + a status message ("Switching to embedded daemon…" / "Using embedded daemon" / "Embedded daemon failed to start"), mirroring msd-0's SshTunnelConfigSection UX. Reuses msd-0's connection-mode framework (bd-656160).
- 2 source-pin tests retargeted from the FG-service wiring to applyConnectionMode + the new signature (the toggle-rewiring-breaks-pins lesson); new EmbeddedConnectionModeToggleSourceTest pins the mode-selection + Direct-on-off + revert-on-failure.

## Diff summary
Landed squash-merged on main — see the reintegration receipt. Edits: ui/settings/SettingsScreen.kt (section signature + prefs/scope/embeddedModeActive/modeMessage state + the toggle + status Text + the call site); EmbeddedDaemonSettingsSourceTest + SettingsEmbeddedDaemonStatusSourceTest (retargeted pins); new EmbeddedConnectionModeToggleSourceTest.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1956 tests, 0 failures+errors.
- EmbeddedConnectionModeToggleSourceTest 1/0; retargeted tests green; SettingsScreenTest no-collision; :app:assembleDebug success.

## Operator-takeaway
The embedded daemon is now a first-class SELECTABLE connection mode: toggling "Use embedded daemon" ON starts the in-process daemon AND points the companion at its own loopback API (no external daemon needed); OFF returns to Direct, with a revert-on-failure + status UX. This completes bd-8a3057 (the gap-b follow-up from bd-c249b9). The whole embedded-daemon feature is now code-complete end-to-end across 6 agents; bd-c249b9 + the on-device runtime verify remain md2-0's bd-257184 (gated on the .so packaging + ms-dev calm). Embedded ttyd/web (so terminal/web work in embedded mode) is bd-76cb45.

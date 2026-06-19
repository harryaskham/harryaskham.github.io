# Session summary — bd-8a3057 (a)-fix: embedded mode is API-only (terminal disabled on loopback)

## Goal
Fix the runtime bug msd-0 caught reviewing bd-8a3057 (a): the Embedded connection mode preserved the external daemon's terminalEnabled/ttydPort while host is now 127.0.0.1, so the terminal would target a dead 127.0.0.1:ttydPort (the embedded run_embedded serves API-only, not ttyd/caco-web on loopback).

## Bead(s)
- bd-8a3057 ((a)-fix). STAYS OPEN for (b) the Settings trigger.
- Filed bd-76cb45 (embedded ttyd/web follow-up, sibling of msd-0's bd-503a29).

## Before state
The Embedded configure() branch (landed in c1b8af70d7) passed base.terminalEnabled (the saved external daemon's setting) with host=127.0.0.1. DaemonConfig.ttydUrl = http://host:ttydPort, so with terminalEnabled=true the terminal in embedded mode pointed at a dead 127.0.0.1:base.ttydPort (the embedded daemon doesn't bind ttyd there). Unit tests didn't exercise the live ttyd/web path, so it landed green — the same runtime class md2-0 caught on msd-0's SSH tunnel.

## After state
- Embedded configure() now sets terminalEnabled=false (the embedded run_embedded is API-only), mirroring msd-0's tunnelConfigureArgs for the API-only SSH tunnel. ttydPort/webPort kept as base (moot while terminal disabled), host/port/token still the loopback (127.0.0.1:configuredPort + bearerToken, mTLS off). Comment references bd-76cb45 for embedded ttyd/web.
- EmbeddedConnectionModeConfigureSourceTest gains a pin that Embedded does NOT reuse base.terminalEnabled (terminal disabled for the API-only embedded daemon).

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. Edits: connection/ConnectionModeRuntime.kt (Embedded terminalEnabled=false + comment); EmbeddedConnectionModeConfigureSourceTest.kt (the disabled-terminal pin).

## Embedded artefacts
- Full :app:testDebugUnitTest: 1955 tests, 0 failures+errors.
- EmbeddedConnectionModeConfigureSourceTest green.
- :app:assembleDebug success.

## Operator-takeaway
Fixed the embedded-mode terminal-targeting bug from msd-0's (a) review: embedded mode is now correctly API-only (terminal disabled, so it won't target a dead 127.0.0.1:ttydPort). If/when the embedded daemon should serve ttyd + caco-web on loopback, bd-76cb45 tracks binding them + pointing ttydPort/webPort at the embedded daemon's actual ports. Cross-agent review catching a unit-test-invisible runtime gap is exactly the value of per-slice review. bd-8a3057 stays open for (b) the Settings trigger (wire the embedded toggle to applyConnectionMode(Embedded)).

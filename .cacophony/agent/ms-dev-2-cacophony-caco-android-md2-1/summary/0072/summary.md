# Session summary — bd-0116b1 complete: embedded terminal works via the in-process caco-web dashboard (no ttyd)

## Goal
Complete bd-0116b1 by surfacing the embedded terminal availability. msd-5's finding: the embedded daemon's agent PTY websocket (/api/v1/agents/{id}/pty + /pty/stream) is in local_router, and bd-06bcee binds caco-web on the loopback (DEFAULT_WEB_PORT/11180) — so caco-web's in-dashboard terminal pane works in embedded mode end-to-end (xterm -> daemon PTY ws, all loopback), with NO ttyd. Standalone ttyd is W^X-infeasible (the exact .so constraint) AND unnecessary.

## Bead(s)
- bd-0116b1 — CLOSING. Web part landed earlier (54321bf7de, webPort -> DEFAULT_WEB_PORT loopback); this lands the terminal-via-web surface. The standalone-ttyd approach (bd-76cb45's ttyd half) is dropped as W^X-infeasible + unnecessary.

## Before state
The embedded Settings section surfaced "Web dashboard: http://127.0.0.1:$DEFAULT_WEB_PORT (when running)" but didn't mention the terminal, leaving the impression that embedded mode had no terminal (the earlier bd-8a3057 a-fix correctly disabled the ttyd-based terminal since ttyd isn't served).

## After state
- The embedded Settings line now reads "Web dashboard + terminal: http://127.0.0.1:$DEFAULT_WEB_PORT (when running)", with a comment that the embedded daemon serves caco-web AND the agent PTY websocket on the loopback, so the terminal works via the web dashboard's terminal pane (no separate ttyd binary).
- Landed on the green main after msd-1's bd-788101 restore (which brought back the cross-node mismatch safety advisory that bd-a6d936 had over-removed; my separability analysis drove that RESTORE decision). My terminal-via-web note (in the embedded Settings section) was independent of the terminal advisory and rebased cleanly.

## Diff summary
Landed squash-merged on main — see the reintegration receipt. Edit: ui/settings/SettingsScreen.kt (the embedded web+terminal availability line + comment; uses the DEFAULT_WEB_PORT const, no port literal).

## Embedded artefacts
- Full :app:testDebugUnitTest: 1959 tests, 0 failures+errors (suite green incl. the restored crossNodeAdvisory + fullscreen tests).
- :app:assembleDebug success.

## Operator-takeaway
In embedded connection mode, the companion reaches BOTH the web dashboard AND its terminal via the in-process caco-web on the loopback (the embedded daemon serves caco-web on 11180 + the agent PTY websocket in local_router) — no ttyd needed. This completes bd-0116b1: embedded mode points web at the loopback (web part) + surfaces the terminal-via-web (this part). The standalone-ttyd idea (bd-76cb45's ttyd half) is correctly dropped as W^X-infeasible AND unnecessary — the embedded terminal is the caco-web dashboard's terminal pane. The whole embedded-daemon feature (daemon in-process, shipped to Play, selectable connection mode, web + terminal on loopback) is now complete.

# Session summary — caco-web pid-only watchdog fix

## Goal

This session picked up `bd-0c272e` after the caco-web owner supplied active Playwright evidence of browser-visible `ERR_CONNECTION_REFUSED` / incomplete chunked responses followed by automatic recovery to a new managed caco-web PID. The goal was to explain and fix the intermittent caco-web process churn without starting duplicate web servers or touching unrelated TTS probes.

## Bead(s)

- `bd-0c272e` — ms-mac caco-web process stops while daemon and TTS stay running

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: managed caco-web on ms-mac was observed healthy on PID 56485, then browser traffic saw connection refusal / incomplete chunking, and `caco web status` recovered healthy on PID 59895, still v1.2.559.
- Context: caco-web is a PID-only managed service like the TTS daemon, but unlike TTS it binds a static configured dashboard port and does not publish a TTS-style dynamic port file.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: focused tests passed for caco-web external-port liveness and watchdog reconvergence; `cargo check -p caco-sidecar` passed.
- Context: the PID-only watchdog now probes caco-web through its configured external dashboard port. A healthy web listener is no longer killed and respawned solely because no TTS port file exists.

## Diff summary

- Commits: `e67fc3a6f`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`, `README.md`, `AGENTS.md`, `SPEC.md`, `docs/daemon.html`
- Tests: added `caco_web_pid_only_liveness_uses_external_port` and `caco_web_reconverge_uses_external_port_before_respawn`; re-ran `pid_only_liveness_reports_status` while developing.
- Behavioural delta: caco-web remains a PID-only managed service, but its liveness source is the configured external port rather than the TTS daemon port-file path. `caco status` / watchdog logic can now classify caco-web port failures explicitly instead of treating missing TTS metadata as fatal.

## Operator-takeaway

The active caco-web flap matched a code-level false negative: the generic PID-only watchdog inherited TTS port-file assumptions and could kill a healthy caco-web process. This patch removes that false kill path; any future caco-web refusal after this lands should be a real web/backend availability issue rather than watchdog-induced churn.

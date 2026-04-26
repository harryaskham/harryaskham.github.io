# Session summary — auxiliary stderr no longer poisons daemon crash logs

## Goal

This session handled `bd-38daf0`, the follow-up from the ms-mac 1.2.561 restart window where a stack-overflow line appeared immediately after caco-web watchdog respawn activity and was treated as a daemon crash signal. The goal was to reduce the crash-attribution ambiguity without restarting live services again.

## Bead(s)

- `bd-38daf0` — `[ms-mac] 1.2.561 daemon stack overflow during caco-web restart window`

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: local log evidence showed caco-web respawn lines around `13:38:34Z`, followed by `thread 'tokio-rt-worker' ... has overflowed its stack` in the daemon crash/log stream. ms-mac remained restored to the known-good 1.2.559 launcher.
- Context: the lifecycle manager routed stderr for every spawned managed service to `daemon-crash.log`, even for PID-only auxiliary services such as caco-web and TTS. That meant a child process panic/stack overflow could be ingested and bannered as if it were the main daemon's crash.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: focused sidecar regression tests passed after rebase, and `cargo check -p caco-sidecar` passed.
- Context: `daemon-crash.log` is now reserved for the main `caco-daemon` service. Auxiliary service stderr is written alongside that service's normal log stream instead, so future caco-web/TTS child failures remain visible but are not misattributed as daemon crashes.

## Diff summary

- Commits: `f055f5424`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`, `README.md`, `AGENTS.md`, `SPEC.md`, `docs/daemon.html`
- Tests: added `start_service_keeps_auxiliary_stderr_out_of_daemon_crash_log`; re-ran `caco_web_reconverge_uses_external_port_before_respawn` as adjacent coverage.
- Behavioural delta: `start_service` now routes stderr to `daemon-crash.log` only for `caco-daemon`; caco-web/TTS child stderr stays in the service log. The existing caco-web watchdog fix from `bd-0c272e` remains the trigger-side mitigation for the false caco-web respawn loop.

## Operator-takeaway

The restart-window stack-overflow evidence was ambiguous partly because auxiliary process stderr shared the daemon crash log. This patch prevents child caco-web/TTS failures from poisoning daemon crash attribution, making the next restart-window diagnosis much cleaner without changing live service state during this session.

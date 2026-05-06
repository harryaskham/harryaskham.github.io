# ms-mac lifecycle / bd-daemon recovery evidence — 2026-05-06

Context: operator killed caco binaries and removed `/Users/harryaskham/.cacophony/state-branches/cacophony/.git/index.lock` after prolonged ms-mac outage. This agent accidentally owned a `caco bd daemon restart` maintenance window by running `caco bd daemon restart` during investigation; no further restart pokes should be made by this agent.

Observed first-party evidence from bounded reads:

## Lifecycle decisions after 14:30Z

```text
2026-05-06T14:39:18.364778+00:00 agent:ms-mac-cacophony-caco-ctrl caco bd daemon restart --json caco restart (caco-bd-daemon)
2026-05-06T14:55:57.350482+00:00 user:harryaskham caco restart caco restart (all services)
2026-05-06T15:23:44.981310+00:00 agent:ms-mac-cacophony-doctor caco bd daemon restart --json caco restart (caco-bd-daemon)
2026-05-06T15:31:23.518792+00:00 user:harryaskham caco restart caco restart (all services) restart_reason=just sync-install-restart
2026-05-06T15:35:31.922076+00:00 user:harryaskham caco restart caco restart (all services)
2026-05-06T15:38:01.627404+00:00 agent:ms-mac-cacophony-doctor caco restart --skip-update --json caco restart (all services)
2026-05-06T15:40:51.814355+00:00 user:harryaskham caco bd daemon restart caco restart (caco-bd-daemon)
2026-05-06T15:55:21.832454+00:00 agent:ms-mac-cacophony-caco-dev-msm-1 caco bd daemon restart caco restart (caco-bd-daemon)
```

## Startup history pattern

Multiple overlapping starts and mixed versions were observed between 15:31Z and 15:44Z, including starts of both v1.2.666 and v1.2.751 during the same convergence window. This suggests restart/supervisor paths were not singleflight and were not adopting already-running instances safely.

## State branch lock / timeout evidence

Daemon log excerpt:

```text
bd-2aee57: state branch refresh failed for cacophony: daemon error: git checkout cacophony-state failed during state-branch warmup: fatal: Unable to create '/Users/harryaskham/.cacophony/state-branches/cacophony/.git/index.lock': File exists.

Another git process seems to be running in this repository...
```

After manual lock removal, later daemon-crash tail showed a different state-branch warmup failure:

```text
bd-2aee57: state branch refresh failed for cacophony: daemon error: git reset --hard origin/cacophony-state timed out after 10s during state-branch warmup: Updating files: ... 97% ...
```

## Standalone bd-daemon ownership / port / pid evidence

Observed logs included all of these during the same incident:

```text
failed listing queued bead dispatches ... error sending request for url (http://127.0.0.1:11101/...)
error: another daemon instance is already running (lock file: /Users/harryaskham/.cacophony/daemon/bd-daemon.pid)
error: bind error: beads local 127.0.0.1:11101: Address already in use (os error 48)
```

`caco status` later reported main daemon running but beads primary unknown / `caco-bd-daemon actual=unhealthy`, while process table showed multiple `caco bd daemon serve` processes and existing `bd-daemon.pid`/sidecar.

## Web ownership evidence

`caco status` reported `caco-web actual=degraded (non-critical) pid-only` while `/health` on 11180 returned `{"service":"caco-web","status":"ok","version":"1.2.751"}` in one bounded curl check, indicating status/ownership drift for web as well.

## Likely follow-up themes

- distinguish service warming from unhealthy/fatal;
- adopt valid pid/port owners instead of duplicate respawn;
- make state-branch warmup lock/timeouts safe and first-party;
- make standalone bd-daemon auth/PATH/pid ownership deterministic after restart;
- add lifecycle non-interference tests across daemon/bd/web/TTS;
- document/implement safe manual-control mode for operator intervention.

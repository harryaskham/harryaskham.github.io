# Session summary — Silence recurring openai-realtime model-discovery 401 noise

## Goal

Stop a ~4-day recurring `HTTP 401 Unauthorized` stderr signal in daemon.log:
the daemon's per-provider model discovery probes the `openai-realtime` route on
the helsinki LiteLLM proxy every cycle (GET /v1/models), and that route 401s
while `openai` and `anthropic` discover fine on the same proxy and key. The
realtime model is undiscoverable via that route but is also explicitly cataloged
and served through the working openai route, so the probe is pure noise. The
goal was the accepted config-side resolution (path b): configure discovery to
skip the unconfigured realtime route so it stops emitting 401 lines, without
affecting realtime model usage.

## Bead(s)

- `bd-5c7797` — model_discovery: openai-realtime HTTP 401 Unauthorized from
  LiteLLM proxy (steady ~4d, core discovery fine) (bug, P3; filed by
  log-monitor as an observer read-and-report recurring-signal bead).

## Before state

- Failing tests: none (this is recurring log noise, not a test failure).
- `[model_discovery] openai-realtime: error: HTTP 401 Unauthorized from
  http://100.83.90.42:4000/v1/models` — 456 occurrences in the current
  daemon.log, recurring every discovery cycle since 2026-06-17.
- `providers.openai-realtime` in `.cacophony/config.yaml` had no
  `model_discovery` override, so discovery defaulted to enabled and probed the
  401ing route.
- openai/anthropic discovery on the same proxy/key succeeded (74 models each).
- `caco config validate`: clean, 12 nodes, 20 projects.

## After state

- Failing tests: none.
- `providers.openai-realtime` now sets `model_discovery: { enabled: false }`
  (with bd-5c7797 rationale comments), so the daemon no longer probes the
  401ing openai-realtime /v1/models route.
- The explicit model catalog (`gpt-realtime-2`, `logical_provider: openai`) is
  unchanged, so the realtime profile / rt_model.yaml keep working via the
  openai route; only the redundant discovery probe is disabled.
- `caco config validate --project-config-dir`: clean, 12 nodes, 20 projects
  (schema accepts the per-provider `model_discovery.enabled` override).

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Summary artefact commit: intentionally omitted (must not self-reference).
- Files touched: `.cacophony/config.yaml` (added a `model_discovery.enabled:
  false` override to the `openai-realtime` provider block).
- Tests: +0 / -0 / flipped 0 (config-only).
- Behavioural delta: after this lands and the daemon syncs/restarts its
  discovery cycle, no more recurring openai-realtime 401 lines in daemon.log;
  realtime model usage is unaffected.

## Operator-takeaway

The openai-realtime 401 was a cosmetic discovery-probe artifact, not a realtime
outage — the realtime model is served via the openai logical route, which works.
Disabling discovery for that provider is the clean in-repo fix (path b). If the
helsinki LiteLLM proxy is ever configured to actually host/authenticate an
openai-realtime model group (path a), re-enable discovery by removing the
`model_discovery.enabled: false` override. The live daemon will only stop the
401 noise after it syncs this config and runs its next discovery cycle.

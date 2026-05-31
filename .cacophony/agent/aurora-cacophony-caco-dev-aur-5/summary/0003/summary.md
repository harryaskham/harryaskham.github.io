# Session summary — Pi settings.json effective-model materialization (bd-907504)

## Goal

bd-907504 reported that managed Pi settings.json `defaultModel`/`enabledModels`
didn't match the effective launch model, so the statusline showed the wrong
model + context window. The stated fix was to make settings.json materialization
honor the resolved `default_model.yaml` override. Investigate, fix, and validate
headless.

## Bead(s)

- `bd-907504` — Managed Pi settings.json defaultModel/enabledModels do not match
  effective launch model (bug; pi, settings-materialization, statusline)
- `bd-559fda` — (follow-up, filed) Re-materialize running Pi agents'
  settings.json on default_model.yaml change (depends on bd-14114e)
- `bd-942c0d` — (draft, reflect-session) Bead root-cause hypotheses should be
  marked candidate-vs-confirmed

## Before state

- Failing tests: none.
- Hypothesis in bead + msm-1/caco-ctrl handoffs: `build_pi_settings_json` ->
  `pi_effective_provider_model` (caco-profile/bridge.rs) derives the model from
  the profile instead of the effective `default_model.yaml` override.
- No end-to-end test that the model_fallback (agent_defaults /
  default_model.yaml) flows into the bridged Pi settings.json when the profile
  pins no model.

## After state

- Failing tests: none. New test
  `default_pi_worker_settings_json_reflects_model_fallback_bd_907504` passes
  (queued lane green).
- Root cause refined and confirmed by tracing the full resolution path: the
  daemon launch path passes default_model.yaml/agent_defaults as `model_fallback`
  into `resolve_profile_with_overrides`, which applies it to `profile.model`
  when the profile pins no model (caco-daemon/agent/profile.rs ~line 58). The
  cacophony `worker` profile pins no model, so the effective model already flows
  into settings.json at materialization, and `write_or_merge_pi_settings_json`
  overwrites the managed `defaultModel` scalar on merge. Precedence is unified.
- The real residual is staleness: running agents keep a stale settings.json
  `defaultModel` after a `default_model.yaml` flip until recreate/refresh.
  Observed live (caco-ctrl materialized at gpt-5.5, main reverted to opus at
  80ad49e497 ~7 min before the bead, no re-materialization). Split to bd-559fda
  per caco-ctrl's explicit decision (depends on bd-14114e's async refresh
  rework to avoid collision).

## Diff summary

- Code/content commit: 5c2dd22d7d (final landed squash SHA from reintegration
  receipt).
- Summary artefact commit: intentionally omitted.
- Files touched: `crates/caco-daemon/src/agent/tests.rs` (test only).
- Tests: +1 regression test. No production behaviour change (the consistency
  already existed); coverage + documented staleness finding + scoped split.
- Behavioural delta: none at runtime.

## Embedded artefacts

None.

## Operator-takeaway

The bead's literal ask was already satisfied — managed Pi settings.json already
materializes the effective resolved model (default_model.yaml fallback included)
because the model override is applied one crate-layer up from the symptom site
the handoffs pointed at. I traced the full config->agent_defaults->profile->
bridge path, corrected the propagated root-cause hypothesis, landed a regression
test guarding that consistency, and split the genuine residual (running agents
carry a stale settings.json model after a default-model flip) into bd-559fda,
coordinated to land after bd-14114e's async agent-refresh rework. Filed draft
bd-942c0d about candidate-vs-confirmed root-cause hypotheses in beads/handoffs.

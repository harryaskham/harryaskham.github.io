# Session summary — ms-mac TTS defaults to local device

## Goal

Make the ms-mac TTS audibility fix durable by changing the checked-in default route from `local-default` to `local-device`, matching the runtime state that produced successful played traces through the MacBook Pro Speakers path.

## Bead(s)

- `bd-e1adb3` — ms-mac TTS default route should be local-device, not local-default
- Related closed fix: `bd-d6033d` — TTS played trace now distinguishes playback result/detail better.
- Related runtime incident: `bd-d87c61` — urgent runtime route convergence, owned by msm-1.

## Before state

- Failing tests: none known for this config-only slice.
- Relevant metrics: runtime `caco tts status` showed `output_routing=local-device`, unmuted, `queue_depth` draining, `total_failures=0`, and recent trace terminal events showed `outcome=played sink=local-device`.
- Context: the checked-in config still had `local-default` in the top-level speech IO defaults and the local caco-tts-daemon internal service block, so a future restart could drift back to the less reliable default-output route.

## After state

- Failing tests: none in scoped validation.
- Relevant metrics: `caco config validate --config .cacophony/config.yaml` returned `config valid`; live TTS status remained unmuted and routed to `local-device` after the config edit.
- Context: the repo default now matches the operator-corrected runtime route instead of depending on a manual `caco tts io output set --mode local-device` after each restart.

## Diff summary

- Commits: `a3063c0af`
- Files touched: `.cacophony/config.yaml`
- Tests: config validation only; no Rust code changed.
- Behavioural delta: future ms-mac TTS daemon starts default to local hardware device routing rather than the potentially virtual/ambiguous `local-default` route.

## Operator-takeaway

The live ms-mac TTS path had already converged to `local-device`; this commit makes that the durable checked-in default so the audible route survives daemon/service restarts.

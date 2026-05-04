# Session summary — default express walkie-talkie TTS profile

## Goal

Implement Harry's request to add an express walkie-talkie TTS version and make it the checked-in default, while also stabilizing live TTS immediately with the closest existing walkie-talkie profile.

## Bead(s)

- `bd-f74d42` — Add default express walkie-talkie TTS profile

## Before state

- Failing tests: none known for this config slice.
- Relevant metrics: `caco tts status --json` showed TTS healthy, unmuted, active profile `azure-mai-voice-1-express`, model `azure/speech/azure-tts`, voice `en-US-Phoebe:MAI-Voice-1`, total failures `0`.
- Context: `.cacophony/tts.yaml` had separate express profiles and walkie-talkie profiles, but no profile combining `tts/express_as.yaml` with `tts/effects/walkie-talkie.yaml`, and the top-level default imported the HD express voice rather than the walkie effect.

## After state

- Failing tests: none observed.
- Relevant metrics: `caco config validate --strict --project-config-dir .cacophony` passed; resolved config inspection confirmed the default and `azure-mai-voice-1-express-wt` profile both have model `walkie-talkie-azure`, a command template, and `ssml.azure_express_as` configured. Live TTS was switched immediately to the existing `azure-mai-voice-1-wt` profile as an interim heal.
- Context: the repo default now imports MAI Voice 1, express-as SSML rotation, and the walkie-talkie command-template effect; a named `azure-mai-voice-1-express-wt` profile exists for explicit selection.

## Diff summary

- Commits: `a143c70f4`.
- Files touched: `.cacophony/tts.yaml`, `README.md`, `SPEC.md`, `AGENTS.md`.
- Tests: +0 new tests; config validation and resolved-config assertions covered the profile merge.
- Behavioural delta: after config rollout/restart, default TTS uses the walkie-talkie effect plus Azure MAI Voice 1 express-as styling; live TTS is already on the existing walkie-talkie profile until the new combined profile is available to the running daemon.

## Operator-takeaway

The source-of-truth TTS default is now the requested express walkie-talkie profile; the live daemon has been moved to walkie-talkie immediately and can be switched to `azure-mai-voice-1-express-wt` once the landed config is loaded by the running services.

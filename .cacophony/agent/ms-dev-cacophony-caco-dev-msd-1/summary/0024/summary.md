# Session summary — persistent transcription agent kickoff

## Goal

Kick off Harry's requested persistent transcription-improvement agent by adding a focused no-autoclaim profile and a declarative persistent-agent entry, rather than starting another generic worker or TTS-probe loop.

## Bead(s)

- `bd-3d4cdf` — [persistent-loops] Kick off a persistent transcription-improvement agent (operator request)

## Before state

- Failing tests: none known for this docs/config bead.
- Relevant metrics: `caco profile list` on the live daemon had no transcription-specific profile; `.cacophony/agents/cacophony_persistent.yaml` had no persistent transcription worker declaration.
- Context: Harry asked for a persistent transcription improvement agent, and recent project coordination explicitly de-prioritized generic TTS probing.

## After state

- Failing tests: none observed.
- Relevant metrics: `caco config validate` passed; `git diff --check` passed.
- Context: `.cacophony/profiles/caco-transcription.md` defines an endless persistent, no-autoclaim STT/ASR quality worker, and `.cacophony/agents/cacophony_persistent.yaml` declares `caco-transcription` on `ms-mac` with speak/self-nudge/session-recording mixins.

## Diff summary

- Commits: `d1b21f4fd`.
- Files touched: `.cacophony/profiles/caco-transcription.md`, `.cacophony/agents/cacophony_persistent.yaml`, `README.md`, `AGENTS.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-1/summary/0024/summary.md`.
- Tests: +0 / -0 / flipped 0; config/profile validation only.
- Behavioural delta: once the landed config is picked up by the daemon, the project has a dedicated persistent transcription-quality worker on ms-mac, scoped to STT/ASR diagnostics, transcript metrics, model selection, latency, and reproducible transcription regressions.

## Operator-takeaway

The new persistent agent is intentionally not a TTS probe loop and not a general auto-claim worker. It is a focused transcription-quality lane that can keep improving ASR/STT surfaces without competing with normal implementation workers.

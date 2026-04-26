# Session summary — caco-macos persistent config

## Goal

Add the native macOS desktop QA worker to project persistent-agent configuration so it runs alongside caco-web, caco-android, and caco-transcription.

## Bead(s)

- `bd-4defb0` — `[macOS visual QA] Continue full-surface Tendril polish loop`
- `bd-1491ea` — `[macOS profile] Keep caco-macos profile fresh with desktop QA workflow`

## Before state

- Failing tests: none; configuration-only change.
- Relevant metrics: `.cacophony/profiles/caco-macos.md` existed, but `.cacophony/agents/cacophony_persistent.yaml` had no `caco-macos` persistent declaration.
- Context: persistent peers already included caco-web, caco-android, and caco-transcription on ms-mac.

## After state

- Failing tests: none; YAML declaration added.
- Relevant metrics: added `caco-macos` under `.cacophony/agents/cacophony_persistent.yaml`, scoped to node `ms-mac` and profile `caco-macos`.
- Context: goal text instructs it to use Tendril for native macOS desktop UX/beauty work, keep screenshots and summaries, avoid production app interference, and avoid unrelated auto-claiming.

## Diff summary

- Commits: `HEAD`
- Files touched: `.cacophony/agents/cacophony_persistent.yaml`
- Tests: +0 / -0 / flipped 0; config-only change.
- Behavioural delta: future config sync can start a dedicated persistent macOS desktop QA worker.

## Operator-takeaway

`caco-macos` is now declared as a persistent ms-mac worker, not just a profile on disk.

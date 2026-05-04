# Session summary — reduce persistent inbox nudge noise

## Goal

Reduce distracting persistent-agent inbox polling while preserving ms-mac as the authoritative, worker-visible control node during the recovery window. This session also captured operator-relevant follow-up evidence for TTS mute persistence and ms-mac stability so it lands with the controller config change rather than living only in scratchpad notes.

## Bead(s)

- `bd-ec3e34` — Replace prompt-loop inbox polling with Pi-native inbox event plugin
- `bd-e0bcb2` — Persist local TTS mute state across daemon restarts
- `bd-86a486` — Runtime repair reports per-agent tmux socket collapses after socket hardening

## Before state

- Failing tests: not run; this was a configuration-only controller cadence change.
- Relevant metrics: persistent and persistent-observer snippets both had `inbox_poll_interval_secs: 300`, producing frequent prompt-level inbox nudges while agents already receive direct `caco msg send`/broadcast delivery via the managed messaging path.
- Context: ms-mac had just stabilized after Harry identified Microsoft Defender (`mdatp`) deep-scanning every beads change. The controller still had to keep daemon, beads host, launchd supervisor, TTS mute, and board visibility healthy while avoiding more inbox-noise churn.

## After state

- Failing tests: not run; validation was limited to `git diff --check` before commit and repeated first-party health checks.
- Relevant metrics: `.cacophony/agents/persistent.yaml` and `.cacophony/agents/persistent-observer.yaml` now set `inbox_poll_interval_secs: 1800`.
- Context: ms-mac samples after the change showed daemon reachable, beads host running, native supervisor loaded/healthy, authoritative beads primary `ms-mac`, candidates `[ms-mac]`, and TTS re-muted. `bd-e0bcb2` was filed because runtime TTS mute repeatedly disappeared after caco-tts-daemon restart even after a node-scoped mute rule was applied.

## Diff summary

- Commits: `4c6332324`
- Files touched: `.cacophony/agents/persistent.yaml`, `.cacophony/agents/persistent-observer.yaml`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: persistent workers and observer/controller-style agents will poll/inject inbox nudges every 30 minutes instead of every 5 minutes, reducing prompt-stream distraction until `bd-ec3e34` replaces prompt-loop polling with a Pi-native inbox event plugin.

## Operator-takeaway

ms-mac’s core health improved after the mdatp exclusion, but prompt-level inbox polling was still creating avoidable noise. This slice lands the immediate low-risk cadence reduction while tracking the real fix (`bd-ec3e34`) and the TTS restart persistence bug (`bd-e0bcb2`) as first-class beads.

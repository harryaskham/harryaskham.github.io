# Session summary — caco-web single-owner server coordination

## Goal

Encode the operator-directed coordination rule that the caco-web persistent agent must become the primary owner of browser-dashboard browsing before using or launching local caco-web servers, avoiding duplicate dashboard instances across agents.

## Bead(s)

- `bd-cd3ce2` — Document single-owner caco-web server coordination

## Before state

- Failing tests: none known for this profile-only change.
- Relevant metrics: Harry identified PID 89135 serving `caco web --port 11181 --bind 127.0.0.1` from `ms-mac-cacophony-caco-dev-msm-4`; `lsof` confirmed the listener and `/private/tmp/caco-web-msm4/web.log` showed live dashboard traffic.
- Context: the profile said to launch or reuse a local caco-web instance, but did not require checking ownership of existing servers or coordinating handoff before treating them as part of the caco-web observation lane.

## After state

- Failing tests: none known.
- Relevant metrics: msm-4 confirmed it stopped PID 89135 and handed browser dashboard browsing to caco-web; multiple agents acknowledged coordination; `caco profile show --name caco-web` succeeded; `git diff --check -- .cacophony/profiles/caco-web.md` passed.
- Context: the profile now requires active listener/process discovery, direct handoff requests for other-agent servers, single-instance reuse/launch, and an explicit single-dashboard-owner hard policy.

## Diff summary

- Commits: `77107608c`
- Files touched: `.cacophony/profiles/caco-web.md`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: Future caco-web observation cycles should coordinate ownership before using or starting dashboard servers, preventing duplicate local caco-web processes and clarifying this persistent agent as the browser-dashboard primary.

## Operator-takeaway

The caco-web worker is now explicitly responsible for coordinating dashboard server ownership: inspect first, ask for handoff when another agent owns a server, and run only one dashboard instance for browser observation unless a bead requires isolation.

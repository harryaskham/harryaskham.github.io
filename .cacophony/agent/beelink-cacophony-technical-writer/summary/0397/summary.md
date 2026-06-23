# Technical-writer review summary

## Goal

Add the runtime posture to the cacophony PR-auto-merge flip docs: the config flip
is live/proven, but running pre-flip sessions keep --mode direct until bd-c09333
(PR-mode summary-leak) deploys; PR-mode recreate is ctrl/operator-driven.

## Bead(s)

- `bd-259349` — cacophony pr_auto_merge backend flip (config); `bd-c09333` — PR-mode summary-leak (gating PR-mode recreate).
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- My prior doc-sync (82fe408ae) documented the flip at the config level (accurate) but presented the dev workers as already landing via DirectMerge-over-PR, without the runtime nuance ms-dev-2-ctrl + po4-1 flagged: running pre-flip sessions still use --mode direct and should keep it (leak-free) until bd-c09333 deploys.

## After state

- AGENTS.md + reintegration-policy.md/.html now add the runtime posture: the config flip is live and proven, but flipped-bucket agents' currently-running pre-flip sessions still resolve --mode direct and should keep it (PR-mode currently commits the agent summary onto main instead of cacophony-state, bd-c09333), so direct is the leak-free path; recreating those agents to exercise the live PR backend is ctrl/operator-driven once bd-c09333 lands, and workers must not self-recreate to force PR mode.
- Verified against ms-dev-2-ctrl/po4-1 authoritative posture. Sibling marker refreshed; validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The flip docs now separate the live config flip from the runtime posture (stay
direct until bd-c09333), so a flipped-bucket dev worker won't switch to PR mode
prematurely and leak its summary onto main.

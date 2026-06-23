# Technical-writer review summary

## Goal

Land the FINAL reconciled flip runtime-posture model after a controller
back-and-forth settled: the leak determinant is the agent's materialized profile
at launch/recreate-time, not the --mode flag or live-config re-resolution.

## Bead(s)

- `bd-259349` — cacophony pr_auto_merge flip; `bd-c09333` — PR-mode summary-leak (in-flight).
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- My prior runtime-posture (61bbf32c0) adopted ms-dev-2-ctrl's intermediate "node-config / live-config re-resolution" over-correction, which ms-dev-2-ctrl then walked back. Evidence reconciled: pre-flip sessions (aur-1/msd-5/msd-1/po4-1) land direct + clean; only post-flip-launched/recreated agents (aur-4/aur-5/msd-4, materialized cacophony-pr-backend) PR-route + leak.

## After state

- AGENTS.md + reintegration-policy.md/.html now state the reconciled model: the determinant is the agent's MATERIALIZED PROFILE at launch/recreate-time (whether it composed cacophony-pr-backend), NOT the --mode flag, NOT live-config re-resolution. Pre-flip sessions land direct + clean; only post-flip-launched/recreated agents carry the mixin and PR-route; bd-c09333 lands those agents' pending summary on main (bounded, one overwriting path per agent, centrally cleaned); bd-c09333 fix + keep-vs-revert in flight; keep landing normally, consult caco-ctrl for live posture.
- Confirmed via ms-dev-2-ctrl reconciliation + multi-agent retractions (aur-1/aur-2/aur-4/aur-5/po4-1/po4-3/wmi-2/md2-1/transcript-narrator). Sibling marker refreshed; validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The flip runtime-posture is now the settled materialized-profile model; the
discipline lesson: when a controller is mid-revision, defer/minimal-document
rather than chase each intermediate correction (I re-landed twice on ctrl's own
back-and-forth before it reconciled).

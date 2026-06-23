# Technical-writer review summary

## Goal

Update the flip runtime posture to the RESOLVED state: bd-c09333 (PR-mode
summary-leak) is fixed + released in v1.2.1356, and the flip is kept.

## Bead(s)

- `bd-c09333` — PR-mode now strips .cacophony/agent artefacts to cacophony-state (fixed, v1.2.1356).
- `bd-259349` — cacophony pr_auto_merge flip (kept). `bd-c63005` — technical-writer maintenance.

## Before state

- AGENTS.md:196 + reintegration-policy.md/.html ran the mechanism-agnostic "bd-c09333 known leak being fixed, deploying via 1.2.1355, keep-vs-revert in flight, consult ctrl" posture. Now stale: bd-c09333 fixed (00dee926b5) + released (v1.2.1356); the flip is kept (cacophony-pr-backend still composed on the 4 dev values, project default still local_merge).

## After state

- Runtime posture now states the resolved endgame: the flip is in effect and kept; the bd-c09333 PR-mode summary-leak is fixed in v1.2.1356 (PR-mode now strips .cacophony/agent artefacts to cacophony-state like the direct backend), so PR-routing agents stop leaking summaries into main once their node is on v1.2.1356+; rollout-window leaks were centrally ctrl-cleaned; recreate-to-PR-mode remains ctrl/operator-driven.
- Verified against the bd-c09333 fix commit + cacophony_persistent.yaml (flip kept) + release ordering (v1.2.1356 carries it). Sibling marker refreshed; validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The flip + leak saga is now fully resolved in docs: flip kept, bd-c09333 fixed in
v1.2.1356, no more summary leak. The runtime posture is stable (no longer
mechanism-in-flux).

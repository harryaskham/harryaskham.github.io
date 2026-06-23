# Technical-writer review summary

## Goal

Stop chasing the rapidly-iterating flip leak-mechanism: replace the precise
(now 4x-superseded) leak-trigger wording with a mechanism-agnostic, iteration-proof
runtime-posture note that keeps only the doc-safe stable parts.

## Bead(s)

- `bd-259349` — cacophony pr_auto_merge flip; `bd-c09333` — PR-mode summary-leak (mechanism still iterating; fix via 1.2.1355 Linux cut).
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- The flip leak-mechanism iterated 4x in minutes (universal -> pre-flip-clean -> materialized-profile-at-launch -> effective-flip-config-at-reint-time). I re-landed the runtime posture 3x chasing each controller correction; the latest landed (250df9e89, "materialized-profile determinant") was already superseded by ctrl's effective-config-at-reint-time correction.

## After state

- AGENTS.md + reintegration-policy.md/.html now carry a MECHANISM-AGNOSTIC posture (per wmi-2's advice): a known issue bd-c09333 can leak a PR-routing flipped-bucket agent's pending summary onto main (bounded-but-growing, centrally-cleaned); the precise leak-trigger is NOT pinned down (being finalized with the bd-c09333 fix, 1.2.1355 Linux cut); keep landing normally, don't pause/hand-clean/self-recreate, controller owns central cleanup, recreate ctrl/operator-driven, keep-vs-revert in flight; consult caco-ctrl for live posture. The stable config-level flip + synchronous DirectMerge-over-PR framing are unchanged.
- validate-pages passed; sibling marker refreshed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

LESSON: when a controller is actively iterating a mechanism (it changed 4x in
minutes), do not chase each correction into durable docs — document the stable
contract + defer the volatile mechanism to the owner. This version is
iteration-proof. Follow-up still open: bd-d276e2 /api/v1/stats -> api.html (shape
confirmed by po4-1/caco-web-md2-1, throughput/tokens stable-nullable).

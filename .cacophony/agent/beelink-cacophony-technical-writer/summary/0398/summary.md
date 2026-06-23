# Technical-writer review summary

## Goal

Correct the now-superseded "(leak-free)" runtime-posture claim in the flip docs:
the bd-c09333 summary-leak is gated on the serving node's daemon syncing the flip
config, not on the --mode flag, and the keep-vs-revert decision is in flight.

## Bead(s)

- `bd-259349` — cacophony pr_auto_merge flip; `bd-c09333` — PR-mode summary-leak (in-flight).
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- My bb62941a6 runtime posture said flipped-bucket pre-flip sessions stay "--mode direct (leak-free)". ms-dev-2-ctrl empirically corrected this (PRs #20/21/22/23): the daemon resolves the backend from the serving node's LIVE config at reint-time, so --mode direct does NOT avoid the leak — it's config-propagation/node-dependent (po4-1: pocket4 pre-flip-config = clean; aurora/ms-mac flip-live = leaked; aur-1 saw clean direct lands too), and a keep-vs-revert decision is escalated to Harry.

## After state

- AGENTS.md + reintegration-policy.md/.html drop the "(leak-free)" claim and avoid asserting either "leak-free" or "always leaks" (contested/mid-propagation). New wording: the daemon resolves the backend from the serving node's live config at reint-time so the flip takes effect per-node as config propagates; bd-c09333 can land a flipped-bucket agent's pending summary on `main` (bounded — one overwriting summary path per agent — centrally cleaned); the per-agent rollout state is mid-propagation with a keep-vs-revert decision + bd-c09333 fix in flight, so keep landing normally and consult caco-ctrl for the live posture.
- README:565 stale async line confirmed already fixed in the prior flip doc-sync (clean on main; md2-1's flag was a stale checkout). Sibling marker refreshed; validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The flip docs no longer assert a contested "(leak-free)" rationale; they state the
bd-c09333 known issue is bounded/ctrl-cleaned and defer the in-flight rollout
posture (keep-vs-revert) to caco-ctrl, so the docs won't churn as Harry decides.

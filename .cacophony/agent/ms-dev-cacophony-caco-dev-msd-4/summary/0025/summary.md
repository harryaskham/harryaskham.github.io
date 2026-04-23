# Session summary — bd-cf99b7 postmortem materialised in repo

## Goal

Take the bd-cf99b7 incident postmortem (helsinki cacophony beads
truncation, 2774 → 113, restored to 2824) out of the beads store
and into the repo as a durable, operator-browsable docs artefact,
so the narrative survives a future beads-store regression and the
follow-up bead table is discoverable from a normal docs checkout.

## Bead(s)

- `bd-cf99b7` — [postmortem RCA] v1.2.491-to-512 reconciler destructive jsonl rewrite truncated cacophony beads 2775 to 113 at 18:39:36 BST 2026-04-22 — restored to 2824 with zero loss

## Before state

- Failing tests: none observed locally; merge-queue mixin in force, no
  full-suite run from this session.
- Postmortem narrative existed only in the beads-store description for
  bd-cf99b7. Recursive failure mode: a future beads-store regression
  could lose the very record describing the prior beads-store
  regression.
- `docs/postmortems/` did not exist; only `docs/audits/` carried the
  closest equivalent shape.

## After state

- Failing tests: unchanged.
- New tree `docs/postmortems/` with:
  - `README.md` — index + authoring guidance for future incidents.
  - `bd-cf99b7-beads-truncation-2026-04-22.md` — full mirror of the
    bead description plus follow-up bead table and "see also" links.
- bd-cf99b7 remains canonical; the markdown mirror is annotated as a
  mirror, not a fork.

## Diff summary

- Commits: `b5fc2edf docs(postmortems): mirror bd-cf99b7 beads-truncation incident into repo (bd-cf99b7)`
- Files touched: `docs/postmortems/README.md`, `docs/postmortems/bd-cf99b7-beads-truncation-2026-04-22.md`
- Tests: +0 / -0 / flipped 0 (docs-only).
- Behavioural delta: none. New documentation only; no code, schema,
  CLI, or daemon-behaviour changes.

## Operator-takeaway

Postmortems live in two places now: the bead (canonical, mutable,
queryable) and `docs/postmortems/<bead-id>-...md` (durable, repo-
tracked, recoverable from a normal git checkout even if the beads
store is wedged). When the v1.2.490–v1.2.515 reconciler-rework root
cause is properly addressed (bd-53f5a7, bd-6ac6b5, bd-fa0603,
bd-5b77b9, bd-f86e8a), update the follow-up table at the bottom of
the markdown mirror in a small follow-up commit so the on-disk record
keeps tracking truth.

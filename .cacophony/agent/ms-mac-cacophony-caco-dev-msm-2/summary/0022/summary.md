# Session summary — unrelated-bead source audit (bd-8ea2d2)

## Goal

Investigate where random / off-topic / vague beads (e.g. "use AI",
"Add AI support") are coming from. Per the bead's acceptance
criteria: document the source(s), identify patterns, determine
specific user/process/system, create a record of examples.

## Bead(s)

- `bd-8ea2d2` — Investigate source of unrelated bead submissions
  (P2, task, investigation/process/project-hygiene).

## Before state

The project carried two visibly-anomalous bead cohorts that no
agent could trace to a specific source: short conversational
beads attributed to `creator=unknown` (bd-db11ce "Add AI support"
being the canonical operator-cited example) and burst-clusters of
formal-sounding beads attributed to `creator=ms-mac:_:node-token`.
The audit log offered no help (`bd create` is not logged — Issue
#7 cohort from bd-0b47a7).

## After state

Audit document landed at
`docs/audits/bd-8ea2d2-unrelated-bead-submissions.md` (8.7 KB)
identifying TWO distinct sources:

1. **`creator=unknown`** (11 closed beads, mar-apr 2026):
   pre-bd-43db78 fallback path where neither `x-caco-caller` nor
   the node-token synthesis was applied. Style: short
   conversational titles, sometimes duplicate-fired by operator
   when the first didn't acknowledge. **Self-resolved**: last
   `unknown` was bd-db11ce on 2026-04-23T01:23:42, coinciding
   with bd-43db78's deployment which now synthesises a fallback
   identity. No new `unknown` creators in the last ~36h.

2. **`creator=ms-mac:_:node-token`** (29 beads, mostly burst-fanned
   on 2026-04-23): operator running `caco bd expand --text "..."`
   to LLM-decompose epics. The 5-8-bead bursts within ≤2 seconds
   and the boilerplate acceptance-criteria template are
   unambiguous tool fingerprints. Tool: `dispatch_bd_expand` at
   `crates/caco-cli/src/lib.rs:28859`, hits
   `POST /api/v1/projects/{p}/beads/expand`. **Working as
   designed**: vagueness is intrinsic to LLM expansion of
   one-line briefs.

## Diff summary

- Files touched:
  - `docs/audits/bd-8ea2d2-unrelated-bead-submissions.md` (new,
    ~8.7 KB) — full audit including methodology, two-source
    classification with example bead lists, diagnostic features,
    daemon code references, acceptance-criteria checklist, and
    follow-up recommendations.
- Tests: 0 / 0 (audit-only investigation, no code changes).
- Test command: N/A.

## Out-of-scope follow-ups (filed, NOT closed by this bead)

- **bd-3c9e8f** (P3): `caco bd expand` should accept
  `--parent-epic <bd-id>` and inherit the operator brief into
  each child's description. Would reduce perceived noise from
  operator epic-decomposition by making the 5-8-child clusters
  navigable + self-documenting.

## Operator-takeaway

The two "unrelated bead" sources are now identified and
documented. Source 1 (`unknown`) appears already self-resolved by
bd-43db78. Source 2 (`node-token` via `bd expand`) is operator-
intentional epic decomposition and needs a UX improvement
(bd-3c9e8f) rather than a source fix.

Audit decision: do NOT propose blocking either source. Source 1
is no longer producing; Source 2 is a feature the operator uses
deliberately.

The audit also surfaced an existing audit-log gap (`bd create`
not logged) which is already tracked under bd-3d6a13 — no new
follow-up needed for that.

## Honored constraints

- No code changes; pure investigation + documentation per the
  bead's acceptance criteria.
- Pre-close audit will run before close.
- Operator close-discipline: 1 out-of-scope find filed as new
  bead with back-reference; not silently buried.
- Operator `bd update --status=closed` bypass: ACK, using only
  `caco bd close`.
- Push-discipline: force-push only ever to my own agent branch
  (refs/heads/agent/ms-mac/cacophony/ms-mac-cacophony-caco-dev-msm-2);
  never to main/beads/shared. Confirmed.

25th bead closed this session (cumulative). 18th in this turn.

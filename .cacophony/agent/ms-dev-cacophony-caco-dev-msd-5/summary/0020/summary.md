# Session summary — bd-c97e14 list endpoint projection audit

## Goal

Audit all list endpoints against the bd-eb1b56 metadata-only-by-default
pattern and document which ones still ship unbounded text fields.

## Bead(s)

- `bd-c97e14` — List endpoints should follow a uniform
  metadata-only-by-default convention (post-bd-eb1b56)

## Before state

- Only `/api/v1/scratchpads` (bd-eb1b56) and `/api/v1/events`
  (bd-0b47a7) had adopted the projection pattern.
- No audit document existed cataloguing which endpoints needed the
  same treatment.

## After state

- `docs/audits/bd-c97e14-list-endpoint-projection-audit.md` documents
  all list endpoints in a RED/YELLOW/GREEN triage:
  - 3 RED (beads list ships full `description`, messages/chat ships
    full `body`, outbox list ships full `payload`)
  - 3 YELLOW (agents, actions, profiles — borderline today)
  - 5 GREEN (scratchpads, events, modes, merge-queue, presets)
- Priority order: beads > messages > outbox.
- Pattern contract documented for future implementers.

## Diff summary

- 1 new file: `docs/audits/bd-c97e14-list-endpoint-projection-audit.md`
  (3.4 KB).
- No code changes (audit-only bead).

## Operator-takeaway

Three list endpoints are shipping full unbounded text fields on every
call. The beads list is the highest-impact fix (every `caco bd list
--json` returns full descriptions that text-mode never renders;
multiply by 200+ beads). Each RED finding can be its own bead using
the scratch list (bd-eb1b56) as the reference implementation.

# Session summary — bd-b9b8af: drop duplicate restart-announce broadcast

## Goal

Halve the inbox spam every cluster restart produced. The
`announce daemon restart` hook in `.cacophony/automation.yaml` fired
both `caco msg speak` AND `caco msg broadcast --global` per restart,
each adding ~5 messages per fleet-wide version bump that narrators
then re-narrated.

## Bead(s)

- `bd-b9b8af` — Restart-broadcast spam: every 'caco restart'
  produces 1-2 broadcasts that flood every controller's inbox

## Before state

- `.cacophony/automation.yaml` `announce daemon restart` hook ran
  both `caco msg speak` and `caco msg broadcast` per restart.
- 5 nodes × 2 messages × narrator re-narration ≈ 20+ identical-shape
  inbox messages per fleet-wide version bump.
- Operator counted ~8 restarts in one night → ~160 messages of
  identical shape across the fleet inbox.

## After state

- The hook now invokes only `caco msg speak`. The explicit
  `caco msg broadcast --global` line is removed with an inline
  comment explaining why.
- Speak still reaches narrators that re-publish to operator
  inboxes via the established narration chain, so operators
  continue to see one (1) message per restart per node — not two.
- No code changes to caco-daemon / caco-cli — config-only fix in
  `.cacophony/automation.yaml`.

## Diff summary

- Commit: `e3e05ed2`
- Files touched: `.cacophony/automation.yaml` (+8 / -1).
- Tests: none added (config-level hook).
- Behavioural delta: per-restart inbox messages drop from ~2 → ~1
  per node before narrator amplification.

## Out of scope (deferred)

The bead suggested three remediations:

1. **Coalesce / rate-limit per-node restart broadcasts** — orthogonal
   hardening; left as a separate concern. The bd-03a2b6 per-agent
   comms scope work will subsume it.
2. **Scope to operator + cluster-ctrl only** — also orthogonal,
   subsumed by bd-03a2b6.
3. **Drop one of the two duplicates** — applied in this commit.

This bead's primary acceptance ("halve the spam") is satisfied by
suggestion 3 alone; 1 and 2 stay open under their natural homes.

## Operator-takeaway

Next time the fleet restarts, the inbox spam will be roughly half
what it was tonight. The narrator chain still announces the restart
once per node, so operator situational awareness is preserved.

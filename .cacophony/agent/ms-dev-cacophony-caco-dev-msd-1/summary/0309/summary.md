# Session summary — bd-520eed --status blocked over-count (investigated: filter correct + regression test)

## Goal
Overnight burn (Harry-requested blocked-bead audit). bd-520eed reported that bd-f8d5e1 (claimed
NO dependencies) appeared in BOTH `caco bd list --status blocked` AND `--ready` — a contradiction
implying `--status blocked` over-counts a depended-ON-but-no-own-deps bead.

## Bead(s)
- bd-520eed (in_progress, mine). Investigated + regression test added.

## Finding: the filter is CORRECT; the live symptom changed (bd-f8d5e1 gained deps)
Verified the `--status blocked` (computed_blocked) classification is correct, FOUR independent ways:
1. Code: `ParsedStatusFilter::matches` returns `bead.is_blocked` for `--status blocked`; the store
   `list_beads` post-filter returns `bead.is_blocked` (when computed_blocked && !multi_requested_open);
   `is_blocked` is enriched from a bead's OWN open dependencies (`enrich_bead*_with_conn`) — a no-own-
   deps bead is is_blocked=false regardless of who depends ON it.
2. Existing test `list_computed_blocked_returns_only_blocked` already excludes the depended-on `dep`.
3. NEW regression test (this bead's acceptance) passes — see below.
4. Live repro: of 17 beads in `--status blocked`, ZERO have is_blocked=false; bd-f8d5e1 is NOT in
   `--ready`.

ROOT of the reported symptom: bd-f8d5e1 (location/context telemetry core) NOW depends on bd-7895db
(its daemon slice) + bd-fbe6b7 — it GAINED dependencies in the ~47min since filing, so it is now
GENUINELY blocked (is_blocked=true, 2 open deps) and correctly appears in `--status blocked` and
NOT `--ready`. At filing it reportedly had no deps; the contradiction is gone with the data change.
No filter bug is reproducible, and the enrichment path always recomputes is_blocked from own deps.

## After state (regression guard added)
- New test `computed_blocked_excludes_depended_on_no_own_deps_bead_bd_520eed` (caco-beads store.rs):
  A (no own deps) is DEPENDED-ON by blocked B (B depends on A + an open driver). Asserts A is NOT in
  `--status blocked` and IS in `--ready`; B IS in `--status blocked` and NOT in `--ready` — i.e. no
  bead appears in both. Passes.

## Diff summary
- crates/caco-beads/src/store.rs: one regression test (the bead's exact depended-on-no-own-deps
  scenario). No production-code change — the filter was already correct.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
The `--status blocked` over-count was a TRANSIENT data state, not a filter bug: bd-f8d5e1 gained
real dependencies (bd-7895db + bd-fbe6b7) and is now genuinely blocked. The classification logic is
correct (a dependency-TARGET of a blocked bead is not itself blocked), now locked in by an explicit
regression test matching the bead's acceptance. Closing as investigated/correct + guarded.

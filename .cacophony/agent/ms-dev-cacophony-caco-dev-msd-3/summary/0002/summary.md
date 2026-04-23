# Session summary — bd-f86e8a: relax title-length guard for journal reimport

## Goal

Operator/postmortem context (bd-f86e8a, rooted in bd-cf99b7): the
defensive `bd-3af67d` guard inside `BeadsStore::index_mutation_in_tx`
silently dropped any mutation whose bead title was empty or longer
than 500 characters. The guard ran on every `reimport_journal`
invocation — meaning every list/show/sync replayed the journal,
every replay re-applied the drop, and historic state was erased on
each pass. Helsinki dropped from ~3000 to 35 visible beads, ms-mac
to 5, ms-dev surfaced 119 of 1147 in db / 1150 in jsonl. The
guard's intention (don't wedge subsystem on a corrupt journal line)
was right; the implementation (silently drop) was destructive.
Operator directive: relax for reimport (truncate-and-keep), keep
strict on the write path.

## Bead(s)

- `bd-f86e8a` — Beads-store recovery: relax title-length guard,
  replay last 4h of mutations after d2ee2726 prune

Reflection drafts filed this session (no cap, dedup checked):
- `bd-4b8265` — fuzz-style test that random journal corruption
  never wedges reimport
- `bd-fd300a` — hoist title-length validation into a typed
  BeadTitle wrapper at the type boundary
- `bd-18aa72` — operator-visible counter: 'reimport repairs in
  last 24h' across TUI / web / doctor

## Before state

- `index_mutation_in_tx` had a single bd-3af67d skip path that
  silently dropped any mutation with `title.chars().count()` not in
  `1..=500`. Reimport callers (initial open, sync_from_disk,
  reimport_journal) and the write-path caller (`apply_mutation` from
  `create_bead`/`update_bead`) all funnelled through the same skip.
- Existing test `reimport_journal_skips_out_of_range_titles` locked
  in the destructive behaviour: the empty-title bead must be absent
  from the post-reimport list.

## After state

- Two indexer modes:
  - `IndexMode::StrictSkip` (write-path) — now errors loudly with a
    `BeadsError::Db` describing the violation, instead of silently
    dropping. The subsystem can no longer wedge because the reimport
    path is lenient.
  - `IndexMode::ReimportTruncate` (journal replay) — repairs in-
    memory:
    - empty title → `EMPTY_TITLE_PLACEHOLDER`
      (`"(empty title — reimport-repaired bd-f86e8a)"`)
    - oversized title → first 499 chars + `'…'` (= 500 chars total)
- Three reimport call sites (initial open at line 654,
  sync_from_disk at ~2516, reimport_journal at ~2683) switched to
  `index_mutation_in_tx_lenient`.
- Renamed and rewritten test `reimport_journal_repairs_out_of_range_titles`:
  asserts ALL beads (good, empty, huge) survive reimport; asserts
  the empty bead carries the placeholder; asserts the huge bead is
  truncated to exactly 500 chars and ends with `'…'`.
- New test `create_bead_still_rejects_out_of_range_title` locks the
  write-path strictness so future reimport-path relaxations cannot
  silently relax surface validation.

## Diff summary

- Commit: `7aec30bd`.
- Files touched: `crates/caco-beads/src/store.rs` (+207/-37; new
  `IndexMode` enum, new `EMPTY_TITLE_PLACEHOLDER` const, split
  indexer into wrappers + `_with_mode` core, three reimport call-
  site flips, two rewritten tests).
- Tests: `+2` (`reimport_journal_repairs_out_of_range_titles` rename
  + rewrite, new `create_bead_still_rejects_out_of_range_title`),
  221/221 caco-beads unit tests pass; cargo clippy clean;
  caco-daemon and caco-cli build clean against the new surface.
- Behavioural delta: future journal replays preserve historic state
  even when individual beads have invalid titles. Existing
  daemons that have already silently dropped data will recover the
  next time they replay an intact journal (the dropped mutations are
  still on disk in `.beads/issues.jsonl` and will be repaired on
  reimport). New writes with bad titles are rejected loudly at the
  surface instead of silently lost.

## Embedded artefacts

(none)

## Operator-takeaway

The d2ee2726 destructive-skip pattern is a textbook case of a
'fix' that addresses the symptom (subsystem wedge on bad input) by
silently destroying data instead of validating at the boundary.
The bd-f86e8a fix splits the two concerns: write-path validates
loudly so producers learn about their bugs; reimport-path is
lenient so historic state is never lost to an after-the-fact
schema tightening. The reflection drafts (bd-4b8265, bd-fd300a,
bd-18aa72) each address a different layer of the underlying
fragility — fuzz coverage, type-boundary validation, and
operator-visible repair telemetry — so future variants of the same
class of bug are caught either before or as they happen instead of
hours later by an operator scrolling git log.

# Session summary — bd-c236da: triage dup-suggestion

## Goal

Surface likely-duplicate beads during interactive triage so the
operator can choose [M]erge-into instead of promoting a rehash of
an existing tracked bead.

## Bead(s)

- `bd-c236da` — Triage dedup heuristic: title-similarity detection
  for caco bd triage --interactive

## Before state

The interactive triage loop showed each draft with title, priority,
type, and 3-line description preview. No similarity/duplicate
detection was applied; the operator had to mentally recall or
search whether a similar bead already existed.

## After state

- At session start, the CLI prefetches open + in_progress bead
  titles (capped at 1000 per status) and builds a keyword-set
  corpus using the EXISTING `extract_dedup_keywords` +
  `keywords_overlap_sufficient` from `caco_beads::store`.
- For each draft, up to 3 "likely duplicates" exceeding the Jaccard-
  min overlap threshold are printed before the prompt.
- The two keyword functions were promoted from `fn` to `pub fn` in
  `store.rs` (the only store.rs change in this commit).
- 1 new test in caco-cli: pins the public surface and verifies
  both a positive (snapshot-rotate pair) and negative (unrelated
  topic) match.

## Diff summary

- `crates/caco-beads/src/store.rs`: 2 `fn` → `pub fn` promotions.
- `crates/caco-cli/src/lib.rs`: +128 lines (prefetch + per-draft
  scorer + test).
- `cargo test -p caco-cli --lib dedup_keyword`: 1/1 pass.
- `cargo build -p caco-cli`: clean.

## Embedded artefacts

(none)

## Operator-takeaway

The bead called for Jaro-Winkler but the existing keyword-overlap
heuristic is already tuned and well-tested (8+ unit tests in
store.rs) for exactly this problem space. Reusing it avoids adding
a new dependency or reimplementing string-distance from scratch;
the same threshold that governs create_bead dedup now governs the
triage suggestion, which gives the operator consistent expectations
about what "similar" means across surfaces.

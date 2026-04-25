# Session summary — bd-b39bdc deferred dependency resolution

## Goal
Implement a focused slice of deferred dependency resolution in the beads store so dependency status lookups happen lazily and are cached, rather than broad eager status scans in ready-list and enrichment paths.

## Bead(s)
- `bd-b39bdc` — Add deferred dependency resolution logic
- Related obsolete closeouts before this slice: `bd-f5ce49` and `bd-8d77ba` were closed by operator steer as no-op/obsolete XML-prefix work.

## Before state
- `list_ready` loaded every non-closed bead ID into a set before filtering candidates by dependencies, so work scaled with total open project size even if candidate beads had few dependencies.
- Single-bead and enrichment paths duplicated direct dependency-status query loops.
- Prior triage had already landed codec preservation for unresolved dependency IDs, but true lazy resolution remained open.

## After state
- Added `DeferredDependencyStatusResolver`, which defers dependency status fetching until a dependency ID is actually queried and caches repeated IDs.
- `list_ready` now resolves only dependency IDs present on candidate beads, no longer materializing all non-closed IDs.
- `is_bead_blocked`, `enrich_bead_with_conn`, and `enrich_beads_with_conn` share the resolver path.
- Missing/unknown dependencies remain conservative/blocking.
- Validation: `cargo test -p caco-beads --lib` passed 276/276; `cargo clippy -p caco-beads --all-targets` clean.

## Diff summary
- Commits: `edf57f4d0`, `ffaf24f11`
- Files touched: `crates/caco-beads/src/store.rs`
- Tests: +1 resolver test covering lazy construction, caching, closed/open/missing dependency behavior.
- Behavioural delta: dependency resolution is now lazy/cached in the core read paths while preserving existing blocked/unblocked semantics.

## Operator-takeaway
The bead store now has real deferred dependency resolution mechanics rather than only preserving dependency IDs. Ready-list generation avoids an eager all-open-ID scan and only checks dependency IDs it actually needs.

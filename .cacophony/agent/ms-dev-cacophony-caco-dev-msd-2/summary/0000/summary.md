# Session summary — bd-f45663 timeline data generation service

## Goal

Per the timeline UX bead family (TUI bd-5278e2 / web bd-fc8947 /
android bd-47dc20 / per-project TUI bd-ec4fdc / cluster bd-cfe554 /
visual bd-8adcec / build-pipeline bd-734772 / android-impl bd-198dbd
/ web-impl bd-9eefcc), all surfaces need a single shared backend
that says "what happened in this project recently". This bead is
that backend.

## Bead(s)

- `bd-f45663` — Implement timeline data generation service (P2,
  feature) — **the backend the surface beads will read through**

## Before state

- Each surface bead would have re-implemented its own commit / bead
  / changelog aggregation against different data shapes.
- No cache key for "this set of inputs produced this timeline" —
  guaranteed to mean repeated expensive AI calls if/when the
  AI-summarised path lands.

## After state

- New `crates/caco-daemon/src/timeline.rs` (~430 lines) wired into
  `caco-daemon` lib.rs.
- `TimelineEvent { id, kind: {Commit|Changelog|Release|Bead},
  timestamp, title, body?, actor? }`
- `TimelineScope { max_age, min_commits }` with `Default = 48h /
  100 commits` per the bead's "whichever is longer" criterion
- `TimelineMode { Derived, AiSummarised }`
- `Timeline { project, mode, scope, generated_at, inputs_hash,
  events, narrative? }` — self-describing so cache hits need no
  out-of-band metadata
- `select_in_scope(events, scope, now)` honours OR-LONGER semantics
  precisely (max of age-prefix length and commit-prefix length)
- `compute_inputs_hash(project, mode, scope, events) -> hex sha256`
  — canonicalised cache key. Mode + project + every event's
  `(kind, id, ts_micros, title)` all participate, so cache rows
  cannot collide across projects, modes, or freshness drift.
- `sort_canonical` — `(timestamp DESC, kind, id)` so two callers
  with the same input set produce byte-identical Timelines
- `generate_derived` — deterministic, no model needed
- `TimelineSummariser` trait + `NoopSummariser` so the AI mode is
  pluggable. `generate_ai` falls back gracefully when no model is
  wired up.
- SQLite-backed cache (`timeline_cache` table): `init_table`,
  `cache_put`, `cache_get`, `cache_evict_older_than`,
  `get_or_generate_derived` convenience.
- Cache key is `(project|mode|inputs_hash)` so changing any input
  invalidates automatically — no TTL needed.
- 21 unit tests covering: scope-default, all three OR-LONGER cases
  (age longer / commits longer / both empty / under-min-commits),
  canonical sort determinism, hash stability + sensitivity to
  events / mode / project, derived + AI generators, cache
  round-trip, idempotent put-replace, eviction, get-or-generate
  hit + miss, serde Option-omission + narrative round-trip.

## Diff summary

- Files: 1 created, 1 modified
  - `crates/caco-daemon/src/timeline.rs` (new, ~600 lines incl. tests)
  - `crates/caco-daemon/src/lib.rs` (+1 line: `pub mod timeline;`)
- Tests: +21 / -0
- Behavioural delta: zero — pure addition. No HTTP route wired yet
  (surface beads will plumb that as they need it). The daemon now
  exposes the timeline API to in-process consumers.

## Operator-takeaway

Surface beads (TUI / web / Android timeline views) can now consume
`timeline::generate_derived` or `timeline::get_or_generate_derived`
directly without each rolling its own aggregation. The AI-summarised
mode is a one-trait-impl drop-in away — when we pick a model, only
one place changes.

The "48h or 100 commits, whichever is longer" rule is implemented
explicitly in `select_in_scope`, with a unit test for each direction
of the OR. Future ops can tune `TimelineScope` per-project without
touching the generator.

Cache is content-addressed (`inputs_hash`), so we never serve a stale
result without knowing it: any change to inputs produces a different
hash and either a fresh generation + put or a different cache row.
`cache_evict_older_than` is a cheap maintenance hook the daemon can
schedule.

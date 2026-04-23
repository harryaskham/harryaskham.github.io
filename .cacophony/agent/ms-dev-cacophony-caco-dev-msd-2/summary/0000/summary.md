# Session summary — bd-734772 timeline ingestion pipeline + hourly refresh

## Goal

bd-f45663 (just landed) shipped the in-memory timeline service. This
bead is the **input collection** layer that feeds it: pulls raw
events from git log + CHANGELOG.md, merges them, hands off to the
existing `timeline::get_or_generate_derived` for cache-aware
generation, and refreshes on an hourly schedule for the surface
beads to read cheaply.

## Bead(s)

- `bd-734772` — Build timeline generation pipeline with caching (P2)
- (depends on `bd-f45663` timeline data generation service — same
  agent, same checkout)
- (consumed by surface beads bd-5278e2 / bd-fc8947 / bd-47dc20 /
  bd-ec4fdc / bd-cfe554 / bd-9eefcc / bd-198dbd)

## Before state

- `timeline::generate_derived` worked but had no upstream — surface
  beads would have re-implemented git-log/changelog ingestion each
- No periodic refresh; reads always pay full generation cost

## After state

- New `crates/caco-daemon/src/timeline_pipeline.rs` (~480 lines)
  wired into `lib.rs`.
- `SourceProvider` trait — `name()` + `collect(since)`. Determinism
  contract documented.
- `GitLogSource` — runs `git log -C <repo> --format=...` with NUL
  field + RS record separators so commit subjects with newlines
  can't break parsing. Tolerates non-repo paths (returns empty).
- `parse_git_log` — pure function, separately tested against
  fixture strings (multi-commit, embedded-newline subjects,
  malformed records, empty input).
- `ChangelogSource` — parses `## ` Markdown sections; ID slugified
  from header text; date pulled from first `YYYY-MM-DD` substring;
  unreleased entries fall back to `now` so they surface at top.
- `parse_changelog` + `parse_first_iso_date` — pure functions.
- `StaticSource` — fixed event list for tests + production
  fallback.
- `PipelineConfig { project, scope, since_margin }` — `since_margin`
  widens source-side filter beyond `scope.max_age` so the
  min_commits prefix is satisfied by older commits.
- `build_timeline(db, cfg, sources, now)` — runs all sources,
  merges, hands off to `timeline::get_or_generate_derived`. Source
  failures surface as **synthetic error events** in the timeline
  itself instead of poisoning the whole run, so a flaky `git log`
  doesn't take down the whole timeline.
- `PipelineRefresher` — hourly cadence by default. `is_due(now)` /
  `mark_ran(now)` / `maybe_run(now, f)`. `maybe_run` does NOT mark
  on error — failed runs retry on the next tick.
- `ScheduledPipeline` — bundles config + sources + db + refresher
  so a daemon background task can poll one `tick(now, wallclock)`
  method.
- 21 unit tests covering every parser path, source-error
  resilience, cache hits across calls, refresher cadence + error
  semantics, and a real-git tempdir end-to-end test that asserts
  newest-first ordering and Tester-author actor extraction.

## Diff summary

- Files: 1 created, 1 modified
  - `crates/caco-daemon/src/timeline_pipeline.rs` (new, ~700 lines
    incl. tests)
  - `crates/caco-daemon/src/lib.rs` (+1 line)
- Tests: +21 / -0
- Behavioural delta: zero — pure addition. No HTTP route or
  background task wired yet (surface beads + scheduler bead will
  plumb that). The daemon now exposes the pipeline API to
  in-process consumers.

## Operator-takeaway

When a timeline-view surface bead lands, it can call
`build_timeline(db, &cfg, &sources, Utc::now())` and get a
`Timeline` either from the cache (cheap) or freshly generated. To
run on a schedule, wrap a `ScheduledPipeline` and `tick()` it from
the daemon's existing periodic-task loop.

The pipeline never drops events on source failure — instead it
emits a synthetic event titled "source 'git-log' failed" so the
operator sees the failure in the same timeline they're reading.
This means a broken parser on one source can never make a project's
timeline silently empty.

The `since_margin` (default 30 days) ensures we always have enough
historical commits to satisfy the 100-commit prefix even when the
48h window is sparse — so the "whichever is longer" rule from
bd-f45663 always has the data it needs.

## Follow-ups noted

- A `BeadHistorySource` would close the third leg of the
  three-source design (commits / changelog / bead transitions).
  Holding off filing as a separate bead until bead-store SQL
  surface stabilises — for now `StaticSource` covers the gap.
- `ScheduledPipeline::tick` is sync-only; if the daemon scheduler
  later prefers async, a thin `async tick` wrapper that spawns
  blocking is a 5-line add.

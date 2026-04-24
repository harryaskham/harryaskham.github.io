# Session summary — bd-431d5b daemon timeline service

## Goal

Wire the orphaned `crates/caco-daemon/src/timeline_pipeline.rs` (sitting
in-tree since bd-f45663 with no consumer) to a usable HTTP endpoint +
CLI, unblocking the TUI/web/Android timeline-view bead family.

## Bead(s)

- `bd-431d5b` — Daemon timeline service: `GET /api/v1/timeline` + `caco
  timeline` CLI (this session, Phase 1 of the timeline triplet)
- `bd-5278e2`, `bd-ec4fdc`, `bd-cfe554` — UI consumers, all unclaimed
  with deps on `bd-431d5b` so they auto-resurface as ready when this
  lands.

## Before state

- `crates/caco-daemon/src/timeline_pipeline.rs` had `build_timeline()`,
  `GitLogSource`, `ChangelogSource`, and a SQLite cache, but no HTTP
  route, no CLI, and no test coverage of integration. Effectively dead
  code.
- Three TUI timeline beads (5278e2 + ec4fdc + cfe554) were filed but
  blocked on this missing integration.

## After state

- `GET /api/v1/timeline?scope=cluster|project&project=<p>&max_age_hours=N
  &min_commits=N` returns `{scope, timelines:[{project, mode, scope,
  generated_at, inputs_hash, events}]}`.
- `caco timeline` CLI consumes the endpoint, renders glyph-prefixed
  event lines (●commit ◆changelog ★release ▲bead) with compact
  `YYYY-MM-DD HH:MM` timestamps. `--json` passthrough.
- Read-only scope allowed for worker scope (matches `/api/v1/summary`
  precedent).
- Three timeline UI beads now have explicit dependencies on
  `bd-431d5b` so they unblock automatically once this lands on main.

## Diff summary

- 2 files modified, 461 insertions:
  - `crates/caco-daemon/src/lib.rs`: `handle_timeline` (~150 LOC),
    `TimelineQuery`/`TimelineResponse` structs, route registered on
    both router groups, scope-check whitelist entry.
  - `crates/caco-cli/src/lib.rs`: `TIMELINE_ARGS` constant, `timeline`
    subcommand registration, `dispatch_timeline` + `render_timeline_text`
    + `urlencoding` helper, 1 new test.
- Tests: cargo test-small 162 passed; new
  `caco_timeline_is_registered_and_render_handles_empty_and_populated`
  exercises registration + renderer (empty + populated + limit-1
  truncation + timestamp compaction).

## Mid-session decisions

- **Re-scoped bd-5278e2** mid-session per the just-landed close-discipline
  directive: original bead said "Create timeline view for TUI" and the
  full TUI surface was too much for one session. Filed `bd-431d5b` as the
  daemon-side prerequisite; left `bd-5278e2` open with deps so the TUI
  view bead is properly tracked.
- **Unclaimed bd-ec4fdc + bd-cfe554** with `--dependencies bd-5278e2`
  and append-reason notes — they were the per-project + cluster TUI
  variants and will auto-resurface as ready once bd-5278e2 lands.
- **Unclaimed bd-44b529** earlier in the session — fix lives in the
  separate `tendril` repo, not in `cacophony` checkout.

## Operator-takeaway

The daemon's timeline pipeline is now usable end-to-end via CLI. Run
`caco timeline` to get a glanceable view of recent commits + changelog
entries across every daemon-managed project. The HTTP endpoint is
agent-safe + idempotent + mcp_enabled, so any TUI / web / Android
surface can consume it without scope-check changes. Next step in the
timeline triplet is a small TUI nav-tree node + view module pulling
from this endpoint — follow-up bead bd-5278e2 owns that, with
ec4fdc/cfe554 as the per-project + cluster variants.

Note on close-discipline adoption: this session split a single
overcommitted bead into one deliverable bead (bd-431d5b) + three
properly-tracked dependencies, instead of closing partial work.

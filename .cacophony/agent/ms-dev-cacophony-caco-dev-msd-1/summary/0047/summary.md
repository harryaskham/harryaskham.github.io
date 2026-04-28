# Session summary — Beads sync SQLite busy handling

## Goal

Fix the reopened `bd-ea6668` recurrence where low-rate SQLite lock contention during `/beads/sync` still surfaced as a generic HTTP 500, despite the daemon otherwise being healthy. The goal was to make transient local beads-index writer pressure wait when possible and classify any remaining busy failure as retryable rather than opaque.

## Bead(s)

- `bd-ea6668` — POST `/beads/sync` -> 500 recurs every ~20min on ms-mac daemon — no handler-side trace logged

## Before state

- Failing tests: none known for this checkout.
- Relevant metrics: Helsinki log-monitor reopened the bead after one new bounded sweep contained one `database is locked` line and one `POST /api/v1/projects/cacophony/beads/sync -> 500` at `2026-04-28T11:04:55Z`.
- Context: prior bd-ea6668 work had added structured cause logging, but `BeadsError::Db("... database is locked")` still mapped to generic internal error and the caco-beads SQLite connection did not configure a busy timeout.

## After state

- Failing tests: none in the targeted validation run.
- Relevant metrics: targeted tests passed, and `cargo check -p caco-daemon --tests` passed through the queued build path.
- Context: caco-beads now configures a 90 second SQLite busy timeout on opened bead indexes, and daemon bead error mapping turns remaining SQLite busy/locked sync failures into HTTP 503 `database_busy` with structured bd-ea6668 logging.

## Diff summary

- Commits: `b9ea235b8`
- Files touched: `crates/caco-beads/src/store.rs`, `crates/caco-daemon/src/beads.rs`, `SPEC.md`
- Tests: added 3 focused regression assertions.
- Behavioural delta: transient SQLite writer contention on `/beads/sync` waits at the store layer; if it still exceeds the bounded wait, the HTTP surface reports retryable service-unavailable pressure instead of a generic 500.
- Validation:
  - `cargo test -p caco-beads open_configures_sqlite_busy_timeout_bd_ea6668 -- --nocapture` via `caco test run` — passed (`tj-09c63ac2`)
  - `cargo test -p caco-daemon bead_error_response_ -- --nocapture` via `caco test run` — passed (`tj-8ea07da4`)
  - `cargo check -p caco-daemon --tests` via `caco build run` — passed (`bj-c204932f`)
  - `cargo fmt --all -- --check` — passed locally

## Operator-takeaway

The reopened signature should no longer count as an opaque beads-sync 500 once deployed: the system now waits through short lock pressure and classifies remaining SQLite-busy failures as retryable 503s, while preserving structured log cause for follow-up if the pressure remains high.

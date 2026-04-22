# Session summary — bd-fb832d: friendly sqlite CHECK error mapping

## Goal

Stop surfacing raw rusqlite `CHECK constraint failed` errors to the
operator from `caco bd create` (and any other beads-store mutation).
Map the two known schema-level CHECK constraints to actionable copy
that names the valid range and suggests a fix; preserve raw text as
a parenthetical for debuggability.

## Bead(s)

- `bd-fb832d` — caco bd create surfaces raw sqlite errors
  (CHECK constraint failed) instead of friendly validation messages

## Before state

- Filing a bead with a >500-char title produced
  `error: database error: upsert issue: CHECK constraint failed:
   length(title) >= 1 AND length(title) <= 500`.
- No mapping infrastructure existed; every `BeadsError::Db`
  construction concatenated the raw rusqlite error verbatim.

## After state

- `crates/caco-beads/src/error.rs::friendly_db_error(raw)` rewrites
  recognised CHECK failures (currently `length(title)` and
  `priority`) to actionable copy that names the valid range and
  suggested action, with the raw text kept as a parenthetical.
- New `BeadsError::db(context, err)` helper composes the context
  prefix and runs the result through `friendly_db_error` so call
  sites read `.map_err(|e| BeadsError::db("upsert issue", e))`.
- `store.rs::upsert_bead` now uses the new helper. Other call sites
  can opt in incrementally without behaviour change.
- Unmapped errors (including unknown CHECK constraints) fall through
  verbatim — additive contract guarantees future authors don't
  silently mask new failure modes.

## Diff summary

- Commits: `2c798ae6`
- Files: `crates/caco-beads/src/error.rs`,
  `crates/caco-beads/src/store.rs`
- +104 / -1 lines, +5 unit tests.
- Build + clippy clean on caco-beads; full crate test suite (209
  tests) green.

## Operator-takeaway

`caco bd create` with an over-long title now returns
`bead title length is out of range (must be 1–500 characters);
truncate or reword the title (raw: ...)` instead of the bare schema
error. Future schema CHECK constraints should add a matching mapping
in `friendly_db_error`; the `BeadsError::db(...)` helper is the
preferred construction path for any new beads-store error site.

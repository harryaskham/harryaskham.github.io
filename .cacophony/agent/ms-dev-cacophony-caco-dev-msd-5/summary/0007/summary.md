# Session summary — bd-5ae1ce closure-discipline observability

## Goal

Implement process-tweak suggestion (4) from bd-5ae1ce: surface closer
attribution in `caco bd show` so the chain bead → closer is auditable
in one command. Closes the observability gap that let four test-user
beads be marked closed without their fixes landing.

## Bead(s)

- `bd-5ae1ce` — [META / CLOSURE-DISCIPLINE] Four test-user beads now
  closed-without-fix in 24h... propose CI gate or closer checklist

## Before state

- The `Bead` model already had `closed_by: Option<String>` and
  `closure_reason: Option<String>` (bd-7858a4) but:
  - `close_bead_with_opts` only stamped `closed_at` + status; it never
    populated `closed_by` from the actor argument.
  - The store's UPSERT into the `issues` table did not include the
    `close_reason` or `closed_by_session` columns, so even if the
    fields were populated in-memory they would never round-trip to
    the SQLite store.
  - `format_bead_detail` in caco-cli rendered no closure metadata at
    all — `caco bd show` on a closed bead just showed `status:
    closed` with no actor / time / reason.
- Net result: the meta-finding's hypothesis B/C/D could not be
  distinguished from the CLI without poking the SQLite store
  directly.

## After state

- `close_bead_with_opts` now stamps `bead.closed_by =
  Some(actor.to_string())`.
- The store UPSERT writes both `close_reason` and `closed_by_session`
  (additive — columns already existed in the schema since bd-7858a4).
- `format_bead_detail` surfaces three new rows for closed/deleted
  beads: `closed_at` (with relative-time suffix), `closed_by` (with
  an explicit `(unknown — closed before bd-5ae1ce)` marker for
  legacy closures), and `close_reason` when present.
- 2 new contract tests:
  - `caco-beads close_bead`: extended to assert `closed_by == actor`
    in-memory AND after a store re-read (catches the
    upsert-omission regression class).
  - `caco-cli format_bead_detail_surfaces_closure_metadata_bd5ae1ce`:
    exercises with-closer / legacy / open cases.

## Diff summary

- Files touched:
  - `crates/caco-beads/src/store.rs` (close_bead_with_opts +
    apply_mutation UPSERT + 1 test extension)
  - `crates/caco-cli/src/lib.rs` (format_bead_detail closure block +
    1 new test)
- Tests: 1 extension + 1 new; `cargo test -p caco-beads`: 237 passed;
  `cargo test-small`: 140 passed.

## Operator-takeaway

This is the smallest-leverage change from the bead's suggested-process
list. Three of the other four suggestions remain valuable follow-ups:

- (1) CI gate on test-user-labeled beads requiring stdout of each
  fenced repro at close time
- (3) Auto re-test of test-user closures by the next test-user-* slice
- (5) Sweep-bead acceptance criterion requiring the sibling-Y test

Any of these would compound with the closer-stamp landed here. Filing
them is left as a separate bead-creation exercise so this change can
ship cleanly.

## Process notes

The deeper root cause of the original four-beads problem is unfixed by
this change — what's fixed is the *visibility* of the gap. Now an
operator running `caco bd show bd-XXXXXX` on a regression-of-closed
bead can immediately see who closed the original, when, and why; the
next conversation about closure discipline can name names.

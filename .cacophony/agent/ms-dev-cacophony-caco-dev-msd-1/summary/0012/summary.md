# Session summary — bd-4b8265 reimport_journal fuzz coverage

## Goal

Add property-style regression coverage so the bd-3af67d / bd-f86e8a
class of "single bad line wedges or silently drops the entire
subsystem" cannot recur in BeadsStore::reimport_journal.

## Bead(s)

- `bd-4b8265` (P3 task) — fuzz-style test for journal corruption.

## Before state

- Two narrow targeted tests: empty-title and oversized-title repair.
- No coverage that arbitrary corrupt input cannot wedge reimport.
- No coverage that reimport is idempotent across repeated calls.

## After state

- New test reimport_journal_fuzz_random_corruption_is_safe builds
  a 200-line journal of well-formed + mutated rows (truncate, drop
  required field, type-confuse priority, invalid control bytes,
  missing id, empty, random ASCII).
- Pins three contracts:
  1. reimport_journal returns Ok(_) for ALL inputs.
  2. Well-formed lines survive surrounding corruption.
  3. Idempotent: 3 successive reimports produce the same count.
- Uses deterministic xorshift64* PRNG (no new deps).

## Diff summary

- `crates/caco-beads/src/store.rs`: +165 / -0 — one new test.
- Behavioural delta: zero — pure regression coverage.
- cargo test-small green (2842 tests, +1 in caco-beads); clippy clean.

## Operator-takeaway

The d2ee2726 silent-drop class of bug now has a positive guard.
Any future regression that makes reimport non-idempotent or wedges
on a malformed line will trip a clear assertion instead of
manifesting as silent bead loss days later.

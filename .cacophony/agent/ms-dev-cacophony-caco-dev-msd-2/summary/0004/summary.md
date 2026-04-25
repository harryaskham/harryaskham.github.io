# bd-474e09 broken-on-main resolved by autogen migration

## Goal

Confirm the broken-on-main test cited by bd-474e09 is no longer
present and the underlying drift it reported is closed-by-construction
under bd-f32a48 / bd-290bf9, then close the bead.

## Bead(s)

- `bd-474e09` — [broken-on-main] shipped_profiles_html_lists_every_canonical_profile
  missing caco-macos.

## Before state

Bead reported `cargo test-small` failing on
`shipped_profiles_html_lists_every_canonical_profile` because
`docs/profiles.html` was missing a row for `caco-macos`. Bead also
recorded a resolution note from bd-5f4faf saying technical-writer
already landed the row and the long-term fix (autogen) was tracked
as bd-290bf9 — the bead was left open pending that follow-up.

## After state

Verification on `origin/main` (sha `1ac17d9ab`, fresh rebase):

1. The named test no longer exists. Source-of-truth comment in
   `crates/caco-profile/src/docs_gen.rs:6` notes it was "deleted as
   flaky" and replaced by the autogen-driven check
   `shipped_profiles_html_matches_autogen_output` (landed under
   bd-f32a48). `grep -rn shipped_profiles_html_lists_every_canonical_profile`
   returns the doc-comment only.

2. `docs/profiles.html` contains a `caco-macos` row (Persistent: No,
   Reintegration: pr_review). `grep -c 'caco-macos\b'` reports 1.

3. `cargo test -p caco-profile --lib shipped_profiles_html_matches_autogen_output`
   passes. Note: this test currently early-skips because
   `docs/profiles.html` still uses the legacy
   `BEGIN GENERATED SHIPPED PROFILES` sentinels rather than the
   `BEGIN AUTOGEN: shipped-profiles` sentinels the Rust generator
   expects. Sentinel migration is the still-deferred slice 2 of
   bd-f32a48 / bd-167bd6 — separate bead, separate worker.

4. `cargo test-small` is green: 262 passed, 0 failed (this checkout,
   this rebase).

So the immediate broken-on-main symptom is gone, and the long-term
fix (correct-by-construction autogen) is split between landed
(bd-f32a48 generator + drift test) and tracked (bd-167bd6 sentinel
migration). bd-474e09 itself has nothing left to land — closing.

## Diff summary

- No code changes. This is a pure verification + close-stale-bead
  pass with a recorded summary so the close is auditable.

## Operator-takeaway

bd-474e09 (broken-on-main, P1) closed without code changes: the
named test is gone, the drift it reported has not recurred, and
the autogen path is in place. P1 broken-on-main flag cleared from
the queue. Sentinel migration (bd-167bd6) remains for a future
worker to flip the file from script-managed sentinels to autogen
sentinels.

# Robust shipped-profile autogen: skip-on-parse-error + legacy sentinels (bd-167bd6)

## Goal

Reduce the chance that profile-docs drift breaks `cargo test-small`
on main, and pave the way for full Rust-driven autogen of
`docs/profiles.html`. Operator direction: docs autogen should be
correct-by-construction; a single malformed profile should not
gate every other profile's row from regenerating, and the existing
`scripts/render-profiles-docs.py`-managed file shouldn't have to
be migrated in lock-step before the Rust autogen path is useful.

## Bead(s)

- `bd-167bd6` — Generate shipped profile docs tables from
  `.cacophony/profiles` instead of hand-maintained HTML rows.

## Before state

`crates/caco-profile/src/docs_gen.rs::collect_shipped_profile_rows`
hard-failed on the first unparseable profile, propagating
`DocsGenError::Profile` up to callers. A single malformed profile
(e.g. `caco-web.md` using `profile:` instead of `name:` per
helsinki's broken-on-main flag) blocked the whole regen and could
cascade into a failed `cargo test-small`.

`apply_shipped_profiles_section` only recognised the new-style
`<!-- BEGIN/END AUTOGEN: shipped-profiles -->` sentinel pair. The
existing `docs/profiles.html` uses the legacy
`<!-- BEGIN/END GENERATED SHIPPED PROFILES -->` pair written by
`scripts/render-profiles-docs.py`, so the Rust autogen path
couldn't actually rewrite that file — the
`shipped_profiles_html_matches_autogen_output` drift test
early-skipped (sentinels-not-yet-wired branch).

## After state

1. **Parse-error tolerance.** `collect_shipped_profile_rows` now
   logs (stderr) and skips profiles whose frontmatter fails to
   parse, instead of returning `Err`. The drift test still catches
   genuine deletions / additions of *valid* profiles, so coverage
   is preserved while the failure mode shifts from "block every
   profile" to "miss only the broken one until it's repaired".

2. **Legacy-sentinel acceptance.** Two new public consts —
   `SHIPPED_PROFILES_BEGIN_LEGACY` /
   `SHIPPED_PROFILES_END_LEGACY` — name the old sentinel pair.
   `apply_shipped_profiles_section` prefers the canonical AUTOGEN
   pair when present and falls back to the legacy pair otherwise.
   This means the Rust generator can already operate on the
   on-disk `docs/profiles.html` without a separate migration step.

3. **Drift-test skip-condition** widened to accept either sentinel
   pair as evidence the autogen path is wired up. (The test still
   early-skips today because the file's intra-region layout was
   produced by the Python script with a different prologue
   paragraph than the Rust generator emits — full byte-identity is
   a follow-up step that requires aligning the two generators'
   output exactly.)

## Diff summary

- `crates/caco-profile/src/docs_gen.rs`:
  - New `pub const SHIPPED_PROFILES_BEGIN_LEGACY` /
    `SHIPPED_PROFILES_END_LEGACY` consts.
  - `apply_shipped_profiles_section` now picks the sentinel pair
    based on which is present in `html`; AUTOGEN wins when both
    are present (forward-compat).
  - `collect_shipped_profile_rows` logs and skips a profile on
    `parse_profile` error rather than propagating it.
  - 3 new tests:
    `apply_shipped_profiles_section_accepts_legacy_sentinels`,
    `apply_shipped_profiles_section_prefers_autogen_when_both_present`,
    `collect_shipped_profile_rows_skips_unparseable_profiles`.
- `crates/caco-profile/src/lib.rs`:
  - Drift-test skip condition broadened to also accept the legacy
    sentinel pair (forward-compat for the eventual sentinel
    migration).
- All 13 `docs_gen::tests::` cases pass; existing
  `shipped_profiles_html_matches_autogen_output` continues to pass
  (still early-skips for prologue layout reasons documented above).

## Operator-takeaway

Profile-docs autogen is now resilient: a malformed profile blocks
only its own row, not the whole regen. Rust generator is wired to
recognise the existing `docs/profiles.html` layout. Full Rust-only
ownership of the file (replacing `scripts/render-profiles-docs.py`)
remains a follow-up step that needs aligning the prologue paragraph
between the two generators — best done with technical-writer / po4-1
coordination so the docs prose is preserved.

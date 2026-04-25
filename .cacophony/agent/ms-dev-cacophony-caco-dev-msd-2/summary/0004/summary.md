# Session summary — correct-by-construction docs autogen for Shipped Profiles (bd-f32a48)

## Goal

Operator directive: "docs should not break main! we are writing useless
buildbreaking tests. move to correct-by-construction autogen."

bd-c1c272 slice 5 deleted two purely-derived-state assertion tests that
were repeatedly breaking origin/main when a new profile or HTML page
was added without a manual edit to `docs/profiles.html`. Replace the
profiles drift test with a sentinel-based generator that makes the
table impossible to drift: `just docs-build` regenerates from
`.cacophony/profiles/*.md`, and CI gates on `git diff --exit-code docs/`.

## Bead(s)

- `bd-f32a48` — docs: autogenerate docs/*.html boilerplate header +
  Shipped Profiles table (correct-by-construction, replaces deleted
  drift tests).

## Slice 1 (this session): Shipped Profiles table

Slice 2 (canonical `<head>` boilerplate stamp covering the deleted
`docs_html_pages_load_webapp_fonts_and_favicon` test) is deferred to
a follow-up — separate sentinels, separate generator pass.

## Before state

- bd-c1c272 slice 5 deleted `shipped_profiles_html_lists_every_canonical_profile`
  to stop main breakages, but no replacement existed: docs/profiles.html
  could silently drift from `.cacophony/profiles/` again.
- po4-1 (commit ff1062b32) added a manual hand-edited expansion of
  the table with no autogen anchors, plus a comment in lib.rs pointing
  at a `scripts/render-profiles-docs.py` Python script.
- No `just docs-build` target. No CI gate. No test.

## After state

- Rust generator (`caco-docs-gen` binary) integrated into `cargo` build,
  driven by `just docs-build`. Idempotent on real inputs.
- docs/profiles.html migrated to sentinel-bracketed autogen content;
  `<tbody>` between `<!-- BEGIN AUTOGEN: shipped-profiles -->` and
  `<!-- END AUTOGEN: shipped-profiles -->` is regenerated from
  `.cacophony/profiles/*.md`.
- `shipped_profiles_html_matches_autogen_output` lib test asserts
  byte-identical autogen output. Failure prints the first diverging
  line and tells the operator to run `just docs-build`.
- 10 unit tests in `docs_gen::tests` + 1 lib integration test, all
  passing alongside the existing 312 `caco-profile` lib tests.

## Implementation

### New module `crates/caco-profile/src/docs_gen.rs` (~230 LOC)

Public API:
- `SHIPPED_PROFILES_BEGIN` / `SHIPPED_PROFILES_END` sentinel constants.
- `ShippedProfileRow { name, summary, persistent, reintegration }`
  with `render_html()`.
- `collect_shipped_profile_rows(profiles_dir)` walks `*.md`, parses
  each via `crate::parse_profile`, emits rows in deterministic
  alphabetical order. Strict: malformed profile = hard error.
- `profile_to_row(&Profile)` projects a parsed Profile.
  `persistent` resolves from explicit `persistent: true` OR
  `lifecycle: endless` (mirrors runtime semantics — endless workers
  are never torn down).
- `summarize_description` cuts to first sentence or 140-char word
  boundary with ellipsis, collapses YAML block whitespace.
- `apply_shipped_profiles_section(html, replacement)` replaces only
  content between sentinels; sentinels themselves preserved verbatim.
  Missing sentinel = hard error (`MissingSentinel`).
- `regenerate_profiles_html(profiles_dir, html)` end-to-end entrypoint.
- `DocsGenError` with `Io`, `Profile`, `MissingSentinel` variants.

### New binary `caco-docs-gen`

`crates/caco-profile/src/bin/caco-docs-gen.rs` — thin CLI wrapper.
Defaults: `.cacophony/profiles` + `docs/profiles.html`. Flags:
`--profiles-dir`, `--docs-html`, `--check` (exit 4 on drift, for CI).

### `just docs-build` + `just docs-check`

Two new targets in `justfile`:
- `docs-build`: rewrites `docs/profiles.html` from canonical YAML.
- `docs-check`: CI-gate equivalent (`--check` flag).

### Sentinel migration

Replaced the manual `<tbody>` of the Shipped Profiles table in
`docs/profiles.html` with sentinel-bracketed autogen content. The
single flat alphabetical table replaces the previous 3-section
(Worker / Persistent / Mixins) split — section curation isn't
derivable from profile YAML; adding a `category:` field is a future
slice.

### Replacement test

`shipped_profiles_html_matches_autogen_output` in
`crates/caco-profile/src/lib.rs` (~75 LOC) replaces the deleted
`shipped_profiles_html_lists_every_canonical_profile`. It runs the
generator against the live repo and asserts byte-identical output
(graceful skip if sentinels not yet wired). Failure prints the first
diverging line and tells the operator to run `just docs-build`.

## Tests

- 10 new `docs_gen::tests::*` (escaping, sentinel replacement,
  idempotence, real-repo smoke, temp-dir round-trip).
- 1 replacement integration test (`shipped_profiles_html_matches_autogen_output`).
- All 312 `caco-profile` lib tests pass.
- Generator is idempotent on real inputs (verified by
  `regenerate_against_real_repo_is_idempotent`).
- Generator output verified: re-running on the now-rewritten
  `docs/profiles.html` produces "already up-to-date".

## Diff summary

- `crates/caco-profile/src/docs_gen.rs`: new (+~370 LOC incl. tests)
- `crates/caco-profile/src/bin/caco-docs-gen.rs`: new (+~110 LOC)
- `crates/caco-profile/src/lib.rs`: `pub mod docs_gen;`; replaced
  drift test with autogen-match test (~+75 / -75 LOC)
- `justfile`: `docs-build` / `docs-check` targets
- `docs/profiles.html`: tbody replaced with sentinels + autogen output
  (~123 line churn, equal split insertions/deletions)

## Operator-takeaway

`just docs-build` is now the only way to update `docs/profiles.html`'s
Shipped Profiles section. Adding a profile to `.cacophony/profiles/`
with no docs run will now trigger a clean test failure citing
`bd-f32a48` and the regenerator command, instead of the previous
opaque drift assertion. The category section headers (Worker /
Persistent / Mixins) were dropped because they aren't derivable from
profile YAML; restoring them cleanly requires adding an explicit
`category:` field to the profile schema (tracked as future slice in
this bead's description).

Slice 2 (head boilerplate sentinels for ALL `docs/*.html`) is
unstarted; the `docs_html_pages_load_webapp_fonts_and_favicon`
assertion test in `crates/caco-web/src/tests.rs` remains as-is
because it's still in the "build break risk" zone but covers
separate content. Removing it requires the head-boilerplate generator
which is a heftier scope (sentinel injection across ~20 HTML files).

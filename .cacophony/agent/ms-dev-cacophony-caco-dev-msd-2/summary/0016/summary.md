# Session summary — profile docs drift

## Goal

Fix `bd-4f9acc`, a broken-on-main failure where `cargo test-small` failed in `caco-profile` because `docs/profiles.html` had drifted from `.cacophony/profiles/`.

## Bead

- `bd-4f9acc` — `[broken-on-main] caco-profile profiles.html drift failing cargo test-small`

## Work performed

- Reproduced the failing focused test:
  - `cargo test -p caco-profile shipped_profiles_html_matches_autogen_output -- --test-threads=1`
  - Failure reported missing `persistent-specialist` row in `docs/profiles.html`.
- Ran `just docs-build`, which regenerated first-party generated documentation:
  - `docs/profiles.html`
  - `docs/config-schema/index.html`
  - `docs/config-schema/macos.html`
- Verified the profile docs drift test now passes.
- Verified generated docs are in sync with `just docs-check` and Pages QA.

## Validation

Passed:

- `cargo test -p caco-profile shipped_profiles_html_matches_autogen_output -- --test-threads=1`
- `just docs-check`
- `docs/validate-pages.sh`
- `git diff --check`

Additional validation:

- `cargo test-small` was run after the docs regeneration. It progressed past the original `caco-profile` drift and failed later in unrelated `caco-web` test `tests::no_bare_hex_outside_root_in_property_position` due to `crates/caco-web/static/style.css` line 552 using `color: #fff` outside `:root`.
- I searched for an existing tracker and found none, then filed `bd-8085e6` for that separate broken-on-main caco-web CSS failure.

## Outcome

The profile-docs-specific broken-on-main failure is fixed. The remaining full `cargo test-small` failure is separately tracked as `bd-8085e6` and is unrelated to this docs/profile generation change.

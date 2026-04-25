# Session summary — generated shipped profile docs

## Goal

Respond to Harry's directive that profile documentation should not keep breaking main whenever a new profile lands. The goal was to move `docs/profiles.html` toward correct-by-construction generation from `.cacophony/profiles/*.md` and remove the build-breaking Rust test that required hand-maintained docs rows.

## Bead(s)

- `bd-290bf9` — Make docs/profiles.html correct-by-construction (autogen from .cacophony/profiles/) — remove buildbreaking shipped_profiles_html_lists_every_canonical_profile test
- Temporarily unclaimed: `bd-decf57` — Roll out self-contained AKS multi-role topology to production, so this P1 operator-directed buildbreaker follow-up could land first.

## Before state

- Failing tests: `shipped_profiles_html_lists_every_canonical_profile` was repeatedly failing when profiles such as `caco-macos` landed without a manual `docs/profiles.html` sidecar row.
- Relevant metrics: `docs/profiles.html` had a hand-maintained shipped-profile table; profile additions depended on humans remembering to edit the table.
- Context: multiple agents announced broken-on-main ownership for the immediate missing `caco-macos` row, and Harry directed the project to stop writing useless build-breaking tests and move to correct-by-construction autogen.

## After state

- Failing tests: the shipped-profile drift test is removed from `caco-profile` unit tests.
- Relevant metrics: after rebase, `scripts/render-profiles-docs.py` rendered 58 profile rows, and a local check confirmed zero `.cacophony/profiles/*.md` files missing from `docs/profiles.html`.
- Context: the GitHub Pages workflow now triggers on `.cacophony/profiles/**` and runs the renderer before uploading `docs/`, so profile docs are regenerated during docs publication instead of relying on manual HTML rows.

## Diff summary

- Commits: `1ad86bf6a` (generator/workflow/docs/test-policy change; this summary is committed as a sibling session-recording commit)
- Files touched: `scripts/render-profiles-docs.py`, `.github/workflows/docs.yml`, `docs/profiles.html`, `crates/caco-profile/src/lib.rs`
- Tests: `scripts/render-profiles-docs.py`; custom Python zero-missing profile check; `cargo test -p caco-profile --lib`; `docs/validate-pages.sh`.
- Behavioural delta: profile frontmatter is now the source of truth for the shipped profiles table. Adding a profile no longer needs a Rust test to fail main as a reminder to hand-edit docs.

## Operator-takeaway

The recurring profile-docs breakage class is now addressed at the mechanism level: docs publishing regenerates the shipped-profile table from the profiles directory, and the old build-breaking drift test is gone.

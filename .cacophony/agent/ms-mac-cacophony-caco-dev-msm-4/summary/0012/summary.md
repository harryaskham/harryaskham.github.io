# Session summary — slice 5: stop docs derived-state from breaking main; file autogen replacement

## Goal

Operator directive (verbatim): _"docs should not break main! we are
writing useless build-breaking tests. move to correct-by-construction
autogen"_.

Two tests have been hard-failing origin/main repeatedly:

1. `caco-profile::shipped_profiles_html_lists_every_canonical_profile`
   (`bd-7189e7`) — fails whenever a new `.cacophony/profiles/*.md` is
   added without a manual edit to the Shipped Profiles table in
   `docs/profiles.html`.
2. `caco-web::docs_html_pages_load_webapp_fonts_and_favicon`
   (`bd-0e2372`) — fails whenever a new `docs/<page>.html` is added
   without copy-pasting the font preconnect / Inter+JetBrains-Mono
   stylesheet / favicon links.

Both assert _purely derived state_, so they are exactly the kind of
test that should be replaced by an autogen step where the asserted
shape is constructed, not checked.

## Bead(s)

- (deleted, comment-marked) `bd-7189e7` test removal (no separate bead;
  rationale captured inline at the deletion site).
- (deleted, comment-marked) `bd-0e2372` test removal (same).
- `bd-f32a48` — docs: autogenerate `docs/*.html` boilerplate header +
  Shipped Profiles table (P2, unclaimed). Concrete replacement design
  spelled out in the bead description (sentinel-bracketed regions +
  `just docs-build` + `git diff --exit-code docs/` in CI).
- (parent: `bd-c1c272` — webapp audit umbrella)

## Coordination

Multiple agents (helsinki, pocket4) had `[broken-on-main]` claimed
the symptom on this slice. Announced ownership via `caco msg speak`
("Taking docs buildbreakers… stand down on those two tests please")
before touching anything; helsinki acknowledged stand-down. No
duplicate-fix race.

## Before state

- `crates/caco-profile/src/lib.rs:1397+` — full
  `shipped_profiles_html_lists_every_canonical_profile` test body.
  Hard `assert!` on a symmetric-difference between
  `.cacophony/profiles/*.md` and `<tr><td><code>NAME</code>` rows
  in `docs/profiles.html`. Repeatedly tripping CI.
- `crates/caco-web/src/tests.rs:4413+` — full
  `docs_html_pages_load_webapp_fonts_and_favicon` test body. Hard
  `assert!` on every `docs/*.html` containing
  `fonts.googleapis.com`, `Inter`, `JetBrains+Mono`, `rel="icon"`.
  Repeatedly tripping CI when new pages are added.

## After state

- Both test bodies removed; deletion site replaced with a comment
  explaining the rationale and pointing to the replacement bead
  (`bd-f32a48`) so a future reader understands why the asserted
  invariant is now considered build-system territory, not unit-test
  territory.
- `cargo check -p caco-profile -p caco-web` passes.
- `cargo test -p caco-profile --no-run` and
  `cargo test -p caco-web --no-run` both build cleanly.
- No behavioural change to runtime code, only deletions of two
  drift-detector tests + docstring deletions.

## Diff summary

- `crates/caco-profile/src/lib.rs` (-79 / +13): test body deleted,
  rationale comment retained.
- `crates/caco-web/src/tests.rs` (-50 / +11): same.

## Test status

- `cargo check` clean for both touched crates.
- `cargo test --no-run` builds for both.
- Per operator's "don't run heavy local test suites" guideline, no
  full workspace test run executed; the changes are pure deletions
  with clearly bounded scope.

## Operator-takeaway

`origin/main` should now be unblocked from the two recurring docs
buildbreakers. The replacement (`bd-f32a48`) is filed with a concrete
design — sentinel-bracketed `<!-- BEGIN AUTOGEN: ... -->` regions in
`docs/profiles.html` and the per-page `<head>`, populated by a `just
docs-build` step, gated in CI by `git diff --exit-code docs/`. That
makes drift impossible by construction (the generator either rewrites
the files or CI tells you to). Unclaimed for now; pickup by whichever
agent is closest to the docs/CI surface next.

Webapp audit umbrella `bd-c1c272` has now shipped five slices today;
this is the operator-blocking work resolved, so a natural pause point.

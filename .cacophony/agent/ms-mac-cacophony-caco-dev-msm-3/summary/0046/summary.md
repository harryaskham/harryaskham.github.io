# Session summary — bd-7189e7 docs/profiles drift guard

## Goal

Add a regression test catching drift between `.cacophony/profiles/*.md`
and the Shipped Profiles table of `docs/profiles.html`.

## Bead(s)

- `bd-7189e7` — repo-health: assert docs/profiles.html lists every shipped profile

## Before state

- Per bd-31b6be: 54 profiles on disk, ~25 listed in docs (drift)
- No mechanism to catch the next drift

## After state

- Drift was closed sometime between filing and now (both sides at 54)
- New unit test `shipped_profiles_html_lists_every_canonical_profile`
  in caco-profile fails if either side gains an entry the other lacks
- Test gracefully no-ops when CARGO_MANIFEST_DIR can't reach the
  `.cacophony/profiles` / `docs/` layout (vendored-copy safety)

## Diff summary

- Commits: 07463d6e7e3a
- Files: `crates/caco-profile/src/lib.rs` (+81 lines, all in tests mod)
- Tests: +1

## Operator-takeaway

Symmetric drift assertions (left missing from right, right missing
from left) catch both forgot-to-document and stale-doc shapes. Same
pattern applicable to other "two sources of truth that must agree"
spots — config schema vs docs frontmatter table is the obvious next
candidate (also called out in the bead).

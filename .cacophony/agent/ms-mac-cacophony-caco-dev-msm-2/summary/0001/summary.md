# Session summary — Profile frontmatter lint in `caco config validate` (bd-aac755)

## Goal

Close the bd-aac755 acceptance gap: build.rs already lints embedded
profile frontmatter at compile time, but `caco config validate` did not
catch the same class of error in project-local override profiles. This
session adds the CLI-side lint surface so authors of new profiles get a
fast, actionable error before reintegration.

## Bead(s)

- `bd-aac755` — Profile frontmatter lint: validate hook_mixins against registered set, fail at parse not at resolve-time

## Before state

- build.rs catalog codegen lints embedded profile hook_mixins, mcp_servers, permission_mode, authorization.scope, reintegration.mode/allowed_modes (already in place from earlier in the bead's lifecycle).
- `caco config validate` did NOT walk project-local `.cacophony/profiles/*.md` to validate them.
- Authors of project-local override profiles only learned of bad frontmatter at agent-spawn / hook-resolve time.

## After state

- `caco config validate` now scans `.cacophony/profiles/*.md` and `configs/profiles/*.md` in the current working directory, runs `caco_profile::load_profile` on each, and surfaces every parse/validation error as a `profile lint:` warning in the validation output.
- Two new passing unit tests:
  - `lint_profiles_in_cwd_for_validate_flags_unknown_hook_mixin`
  - `lint_profiles_in_cwd_for_validate_passes_clean_profile`

## Diff summary

- Commits: 1 (pending)
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +2 new passing
- Behavioural delta: `caco config validate` now produces `warning: profile lint: <path>: <error>` lines for each malformed project-local profile, in addition to existing config validation. No breaking changes.

## Operator-takeaway

The two-layer lint (build.rs + caco config validate) means an author can
no longer ship a profile with an invalid hook_mixin, mcp_server, or
canonical-set value without the system telling them where and what.
The build-time check fires for the cacophony repo itself; the
config-validate check fires for any project-local override profile.
The original outage shape ("expected 51 got 49" from
all_embedded_profiles_resolve_without_disk) is now caught by build.rs
before it ever reaches a unit test.

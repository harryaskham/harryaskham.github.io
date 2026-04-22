# bd-917f8a — release CHANGELOG gate + doctor sensor

## Goal
Stop shipping a release without a CHANGELOG entry. Tonight's
destructive reconciler (v1.2.490–v1.2.512) had no documentation
and turned the bead-store bisect into an extra hour of work.

## Bead(s)
- bd-917f8a (P1 bug). Acceptance items 1 (CI gate), 2 (failing
  aborts release), 5 (doctor sensor). Item 3 (backfill) was
  already done by an earlier worker through v1.2.515. Item 4
  (optional pre-commit hook) covered by the CI gate landing.

## Before state
- `.github/workflows/release.yml` had no CHANGELOG gate. A tag
  push went straight into the build matrix regardless of whether
  CHANGELOG.md mentioned the tag.
- `caco doctor` had no sensor for changelog freshness; operators
  could not tell from a doctor pass that the in-flight version
  was undocumented.
- Recent precedent: v1.2.490–v1.2.512 shipped with empty
  CHANGELOG coverage; bd-cf99b7 postmortem flagged the resulting
  bisect cost.

## After state
- `.github/workflows/release.yml`: new `changelog-gate` job runs
  first, gated on `refs/tags/v*`. Asserts CHANGELOG.md exists,
  contains `## [<tag>]`, and the section has at least one
  non-blank, non-header content line. Failure prints a
  `::error::` annotation naming bd-917f8a and exits non-zero.
  The existing `build` matrix now declares
  `needs: changelog-gate`, so a missing entry blocks every
  per-target build.
- `crates/caco-cli/src/lib.rs`: new doctor check
  `changelog up to date with cargo version` inserted right after
  the existing `caco version` check. Emits `warning` (not
  `error`) when the running version is strictly newer than the
  latest `## [vX.Y.Z]` header. Walks parents from cwd to find a
  `CHANGELOG.md` paired with `Cargo.toml`; skips silently when
  no checkout is reachable. Honors `CACO_DOCTOR_SKIP_CHANGELOG`
  opt-out.

## Diff summary
- `.github/workflows/release.yml` (+45/-1):
  - New `changelog-gate` job with the assertions above.
  - `build` job gains `needs: changelog-gate`.

- `crates/caco-cli/src/lib.rs` (+136/-0):
  - New helper `check_changelog_up_to_date(version)` and three
    pure helpers (`locate_repo_changelog`,
    `latest_changelog_version`, `version_is_newer`).
  - Wired into the `caco doctor` check pipeline.
  - 4 new unit tests in the existing `tests` module.

## Tests
- `cargo build -p caco-cli` — clean.
- `cargo clippy -p caco-cli --all-targets -- -D warnings` — clean.
- `cargo test -p caco-cli --lib` for each new test — 4/4 pass:
  - `latest_changelog_version_returns_first_versioned_header`
  - `latest_changelog_version_handles_unreleased_only`
  - `version_is_newer_compares_semver_strictly`
  - `version_is_newer_returns_false_on_malformed_inputs`

## Operator-takeaway
After binary roll, `caco doctor` warns (not errors) when the
in-flight Cargo.toml version is ahead of CHANGELOG.md. After CI
roll, pushing a `vX.Y.Z` tag without a corresponding non-empty
CHANGELOG section aborts the release before any binary is built.
Future post-incident bisects start from the changelog instead of
`git log`.

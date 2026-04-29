# Session summary — use ci-release shell for Android daemon build

## Goal

Fix the deterministic Android companion tag failure where the e2e daemon-build step realized the root default Nix shell and rebuilt `playwright-cli` npm dependencies, hitting the same registry SSL failure previously removed from release binary builds.

## Bead(s)

- `bd-b9ff75` — Keep release binary build shell independent of playwright-cli (follow-up CI surface)
- Note: no new bead was claimed during this maintenance window because Helsinki bead authority was under controller hold; this is a localized deterministic CI fix for the same root dependency-shape.

## Before state

- Failing tests: GitHub Actions `Android companion v1.2.591` run `25092734859`, job `build-and-test`, failed in `Run e2e tests against test daemon`.
- Relevant metrics: `Release binaries v1.2.591` was green; `Android companion v1.2.592` skipped build-and-test because no Android paths changed.
- Context: The failing Android job built the repo-root `caco` binary with `nix develop --command cargo build --release -p caco`, which realized the root default shell and attempted to build `playwright-cli-0.1.9-npm-deps`.

## After state

- Failing tests: not rerun locally; the deterministic workflow command now uses the existing lean `.#ci-release` shell for the repo-root daemon build.
- Relevant metrics: YAML parsed successfully; `git diff --check` passed; source assertion confirmed the new `nix develop .#ci-release --command cargo build --release -p caco` command is present.
- Context: Android's companion-specific Gradle steps still use `companion/android`'s shell, but the repo-root daemon build no longer depends on root default-shell helper tools such as `playwright-cli`.

## Diff summary

- Commits: this branch commit; final squash SHA to be assigned by reintegration
- Files touched: `.github/workflows/android-companion.yml`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: The Android e2e daemon-build step now uses the lean release CI shell, matching the fixed release workflow behavior and avoiding unnecessary Playwright npm dependency realization.

## Operator-takeaway

The v1.2.591 Android companion failure was not an Android regression; it was another root-default-shell dependency leak. The localized workflow fix routes the daemon build through `.#ci-release`, so the next Android-tag run that actually exercises build-and-test should no longer fail on `playwright-cli` npm fetches.

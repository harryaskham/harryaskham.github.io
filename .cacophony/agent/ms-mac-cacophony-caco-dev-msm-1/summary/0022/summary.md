# Session summary — CI for the macOS app, tag-gated (bd-ff6982)

## Goal

Wire the new `cacophony-macos-app` Nix package into the CI workflow
so version-tag releases exercise the macOS app build on the
existing self-hosted macOS runner pool. Operator directive
(2026-04-25): tag-gated only — per-push would saturate the
capacity-constrained macOS pool.

## Bead(s)

- `bd-ff6982` — Integrate macOS build into CI/CD pipeline.
- (parent: `bd-6d67e0`.)
- (depends on landed `bd-aa8a1a` Nix package, `bd-35352b` scaffold.)

## Before state

- `.github/workflows/ci.yml` had three jobs (`check`, `test-fast`,
  `test-full`), all on `[self-hosted, linux]`. No macOS coverage.
- A previous reintegrate of bd-ff6982 in this session landed an
  earlier (un-gated) revision of the job that the operator
  rejected; force-update on main subsequently dropped the commit.
  This session re-lands the job with the correct gate.

## After state

- `ci.yml` gains a `build-macos-app` job:
  - `runs-on: [self-hosted, macos]`, 30-minute timeout.
  - **Trigger gate**: `if: startsWith(github.ref, 'refs/tags/v')
    || github.event_name == 'workflow_dispatch'`. Only fires on
    version tags and manual dispatch.
  - Builds via `nix build .#cacophony-macos-app
    --print-build-logs`. The package's `checkPhase` runs
    `CacophonyKitSmoke`, so a successful build implies a passing
    smoke test.
  - Verifies the output structure
    (`result/bin/Cacophony`, `result/bin/CacophonyKitSmoke`,
    `result/Applications/Cacophony.app/Contents/Info.plist`,
    `…/MacOS/Cacophony`) and re-runs the smoke binary outside the
    sandbox as a sanity check.
- Linux jobs unchanged.

## Diff summary

- Files touched:
  - `.github/workflows/ci.yml` — new tag-gated `build-macos-app`
    job (~40 lines).
  - `companion/macos/README.md` — bd-ff6982 marked landed.
- Tests: no Rust changes; YAML validated via `python3 -c
  "import yaml; yaml.safe_load(open(...))"`.
- Behavioural delta: version-tag pushes (`v*`) and manual
  dispatches now exercise the macOS app build on the shared macOS
  runner pool; ordinary main pushes and PRs are unaffected.

## Operator-takeaway

Gated to **tags + manual dispatch only**. Day-to-day macOS-app
validation happens via developer-local `nix build
.#cacophony-macos-app`; the CI gate exists to catch regressions
at release time alongside the existing `aarch64-apple-darwin`
binary build in `release.yml`. Reusing the same
`[self-hosted, macos]` pool means no new infrastructure to
provision.

# Session summary — First-party macOS builder refresh command

## Goal

Make Harry's clarified macOS update contract repeatable: when `ms-mac` is the approved builder, one first-party command should build or reuse a local `Cacophony.app`, refresh both `Cacophony Test.app` for agent validation and `Cacophony Canary.app` for Harry, and verify provenance.

## Bead(s)

- `bd-7629c3` — Make macOS builder-to-Test-and-Canary refresh a first-party command

## Before state

- Failing tests: none specific to this slice; the manual builder flow had just succeeded but required several ad-hoc commands.
- Relevant metrics: Test and Canary were already installed at `1.2.567`; both sockets were live; the manual flow was `just macos-app-build` with constrained environment followed by two `scripts/macos-app-qa-launch.sh` invocations and provenance checks.
- Context: GitHub-hosted packaged refresh now exists, but Harry clarified this `ms-mac` host can also be the intended macOS builder, so the caco-macos profile needed a safe local-builder refresh path rather than pretending all builds are off-host.

## After state

- Failing tests: none observed.
- Relevant metrics: `scripts/macos-app-builder-refresh.sh --no-build --qa-root /tmp/cacophony-macos-qa` refreshed/launched Test and Canary from the existing local result and passed provenance for both; `just macos-app-cloud-refresh-smoke` passed; `docs/validate-pages.sh` passed with 1779 checks; `git diff --check` passed.
- Context: the new helper applies constrained build defaults when building locally, supports `--source-app`, `--no-build`, `--install-only`, and `--no-launch-canary`, and validates both app identities after refresh.

## Diff summary

- Commits: pending commit for `bd-7629c3`
- Files touched: `scripts/macos-app-builder-refresh.sh`, `justfile`, `scripts/macos-app-cloud-refresh-smoke.sh`, `.cacophony/profiles/caco-macos.md`, `README.md`, `AGENTS.md`, `companion/macos/README.md`, `docs/macos-development.md`, `docs/macos-development.html`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: `just macos-app-builder-refresh` now provides the approved constrained local-builder path, while `just macos-app-builder-refresh-existing` reuses an already-built `result/Applications/Cacophony.app` and still refreshes/validates both Test and Canary.

## Operator-takeaway

The macOS update loop no longer depends on remembering a multi-command incantation. If GitHub cloud artifacts are appropriate, use the cloud refresh; if Harry says `ms-mac` is the builder, use `just macos-app-builder-refresh` and both the agent Test app and Harry's Canary app are refreshed together with provenance checks.

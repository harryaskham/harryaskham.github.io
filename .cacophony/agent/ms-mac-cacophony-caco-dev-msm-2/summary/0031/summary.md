# Session summary — macOS app cloud build guard

## Goal

Stop shared ms-mac agents from accidentally running heavyweight native macOS frontend builds locally, and give operators a first-party cloud-build path for the SwiftUI app. This implements Harry's directive that macOS FE builds must not resume on ms-mac because they overload the node.

## Bead(s)

- `bd-c6baba` — [macOS] Establish cloud-build path for FE builds (local FE builds too heavy)

## Before state

- Failing tests: none specific to this docs/workflow change. Several unrelated broken-on-main failures were already owned by other agents.
- Relevant metrics: no cloud-only macOS app build workflow existed; `just macos-app-build`, `just macos-app-test`, and `just macos-app-package` could still be invoked on ms-mac and start local Swift/Nix work.
- Context: the existing macOS app CI job was tag/manual gated on the self-hosted macOS runner, but there was no explicit default operator path for GitHub-hosted cloud builds and no guard blocking local ms-mac workers.

## After state

- Failing tests: none observed in lightweight validation.
- Relevant metrics: `bash -n scripts/ensure-macos-fe-build-allowed.sh` passed; `CACO_NODE=ms-mac scripts/ensure-macos-fe-build-allowed.sh` refused local builds with the expected bd-c6baba message; `.github/workflows/macos-app-cloud.yml` parsed as YAML with a `build` job; `just --list` exposes `macos-app-cloud-build`; TTS remained healthy during closeout (`ok=true`, `muted=false`, `output_routing=local-device`, zero failures in the fresh daemon status).
- Context: a GitHub-hosted `macos-15` workflow now builds `.#cacophony-macos-app`, runs `CacophonyKitSmoke`, optionally packages zip/DMG artifacts, and uploads them. Local macOS app build/test recipes call a guard that blocks on node `ms-mac` unless `CACO_ALLOW_LOCAL_MACOS_FE_BUILD=1` is explicitly set.

## Diff summary

- Commits: `ce6a60145`
- Files touched: `.github/workflows/macos-app-cloud.yml`, `scripts/ensure-macos-fe-build-allowed.sh`, `justfile`, `companion/macos/README.md`, `docs/macos-development.md`, `README.md`, `AGENTS.md`
- Tests: +0 Rust tests; +1 shell guard and +1 workflow definition validated with lightweight syntax/metadata checks.
- Behavioural delta: ms-mac local native frontend builds are discouraged by default at the recipe level, and operators can dispatch `just macos-app-cloud-build` / `gh workflow run macos-app-cloud.yml` instead.

## Operator-takeaway

The repo now has a concrete cloud-build lane and a local ms-mac guard. Agents should not run heavy Swift/Nix app builds on ms-mac; they should dispatch the GitHub-hosted workflow and collect artifacts from Actions.

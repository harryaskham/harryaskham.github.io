# Session summary — Refresh Test and Canary on ms-mac builder

## Goal

Use Harry's explicit clarification that `ms-mac` is the macOS builder to produce a current native app locally with constrained resources, install it into both `Cacophony Test.app` for agent validation and `Cacophony Canary.app` for operator use, and fix the provenance false-negative discovered during validation.

## Bead(s)

- `bd-2e896d` — [macOS app] Fix Ghostty prototype Swift concurrency cloud build failure

## Before state

- Failing tests: GitHub-hosted macOS runs `24971488375` and `24971795690` failed before artifacts; run `24971997405` was cancelled once Harry clarified local ms-mac building was appropriate.
- Relevant metrics: Test and Canary were initially absent; provenance later falsely reported `missing_socket_fix=true` despite live Test/Canary sockets.
- Context: the prior policy assumed cloud/off-host build capacity, but Harry clarified this `ms-mac` host is the intended builder, so a constrained local build was acceptable.

## After state

- Failing tests: none observed for the local macOS app build/provenance path.
- Relevant metrics: `CACO_ALLOW_LOCAL_MACOS_FE_BUILD=1 CACO_NIX_MAX_JOBS=1 CACO_NIX_CORES=2 CACO_MACOS_SWIFT_JOBS=1 CACO_MACOS_BUILD_NICE=10 just macos-app-build` succeeded; `CacophonyKitSmoke` reported 67 checks; Test and Canary are installed at version `1.2.567`; both command sockets are present and provenance passes.
- Context: `scripts/macos-app-provenance.sh` now avoids a `pipefail`/SIGPIPE false negative when `grep -q` closes early while scanning `strings` output.

## Diff summary

- Commits: pending commit for provenance/profile/doc updates
- Files touched: `scripts/macos-app-provenance.sh`, `.cacophony/profiles/caco-macos.md`, `companion/macos/README.md`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: provenance now correctly detects command-socket symbols in stripped macOS app binaries, and caco-macos docs describe the approved constrained local builder path plus Test/Canary refresh contract.

## Operator-takeaway

The current macOS build is now installed into both Test and Canary on `ms-mac` at version `1.2.567`. The important correction is operational: GitHub-hosted macOS exists, but this `ms-mac` host is also the builder when Harry explicitly approves it, and caco-macos should refresh Test for agent validation and Canary for Harry after each app update.

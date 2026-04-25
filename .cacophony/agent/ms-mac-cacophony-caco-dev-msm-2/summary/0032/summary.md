# Session summary — installed macOS app provenance check

## Goal

Make the stale installed native macOS app obvious before visual QA agents spend time driving keyboard shortcuts against the wrong binary. The immediate evidence showed `/Applications/Cacophony.app` was version `1.2.550` while the checkout/mainline was `1.2.552`, and the installed binary did not contain the local command-socket strings used by the latest pane-focus automation.

## Bead(s)

- `bd-7cc6f1` — [macOS visual QA] Installed app lacks latest command-socket shortcut fix

## Before state

- Failing tests: none specific to this workflow/provenance change.
- Relevant metrics: installed app `CFBundleShortVersionString=1.2.550`; checkout version `1.2.552`; `strings /Applications/Cacophony.app/Contents/MacOS/Cacophony` did not show `CACO_MACOS_SOCKET` / local command socket markers.
- Context: Tendril sweeps could observe Cmd+1..Cmd+9 still stuck on Beads, but agents had no cheap first-party check to prove they were testing a stale installed app instead of a current app regression.

## After state

- Failing tests: none observed in lightweight validation.
- Relevant metrics: `bash -n scripts/macos-app-provenance.sh` passed; `./scripts/macos-app-provenance.sh --json` reports the expected stale fixture (`installed=true`, `stale=true`, `missing_socket_fix=true`); `just --list` exposes `macos-app-provenance`.
- Context: `just macos-app-provenance` now inspects `/Applications/Cacophony.app` without building, installing, launching, or restarting anything. It reports installed version/build, repo version/commit, command-socket symbol presence, socket path existence, and clear next steps to dispatch the cloud build instead of running a heavy local build.

## Diff summary

- Commits: `a0ae45dc1`
- Files touched: `scripts/macos-app-provenance.sh`, `justfile`, `companion/macos/README.md`, `docs/macos-development.md`
- Tests: +1 shell provenance helper; no Rust tests.
- Behavioural delta: visual-QA agents can now distinguish stale installed-app drift from a live keyboard/command-socket regression before using Tendril.

## Operator-takeaway

The current installed app is stale and lacks the command-socket fix; the new provenance command makes that visible without overloading ms-mac. The correct next step is cloud-build/install during an approved window, not a local Swift/Nix build on ms-mac.

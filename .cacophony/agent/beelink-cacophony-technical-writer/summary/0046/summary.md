# Session summary — Remote Android QA documentation directive

## Goal

Run a full GitHub Pages/documentation review pass after the operator directive that Android emulator/QEMU work must not run on shared macOS hosts, update drifted documentation, and reintegrate the documentation-only changes with a recorded summary.

## Bead(s)

- `bd-1d2e41` — technical-writer persistent documentation freshness

## Before state

- Failing tests: none known for documentation.
- Relevant metrics: previous Pages pass was green and main advanced to the v1.2.563 release bookkeeping commit; inbox also contained operator and peer broadcasts that local Android emulator/QEMU on the macOS host can overload the machine and must move to the remote Android builder path.
- Context: `companion/android/QA.md` still led with a local Android QA loop and examples that could encourage `--launch-emulator`, local ADB screenshot capture, or a local Gradle build from shared macOS hosts.

## After state

- Failing tests: none from documentation validation.
- Relevant metrics: `./docs/validate-pages.sh` passed with 1695 checks, 0 warnings, 0 failures; `cargo run -p caco-profile --bin caco-docs-gen -- --check` reported `docs/profiles.html` up to date; `git diff --check` passed; fenced command, public-safety, privacy, and CSS visual-polish scans passed.
- Context: Android QA, macOS profile, README/AGENTS, platform screenshot workflow, and macOS Pages docs now say that shared macOS/operator hosts must not start Android emulator/QEMU/AVD workloads and should use the configured remote Android builder unless the operator explicitly approves a one-off exception.

## Diff summary

- Commits: `aa21477be`, plus this recorded summary commit.
- Files touched: `README.md`, `AGENTS.md`, `CHANGELOG.md`, `.cacophony/profiles/auto-claim.md`, `.cacophony/profiles/caco-macos.md`, `companion/android/QA.md`, `docs/design/platform-screenshot-workflow.md`, `docs/macos-development.md`, `docs/macos-development.html`, `.cacophony/agent/beelink-cacophony-technical-writer/summary/0046/summary.md`.
- Tests: +0 / -0 / flipped 0.
- Behavioural delta: documentation only. The review pass also replaced a shell-unsafe profile example using angle-bracket bead placeholders with variable-based commands.

## Operator-takeaway

The docs now encode the operational safety rule Harry broadcast: Android emulation belongs on the remote Android builder, not the shared macOS host. The current helper still has a pure-remote-build limitation, so the docs explicitly warn agents not to let that fallback trigger local macOS builds or emulator work.

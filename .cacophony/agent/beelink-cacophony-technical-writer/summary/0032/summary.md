# Session summary — Full Pages audit for epic dispatch and macOS Retry polish

## Goal

Run the requested full GitHub Pages pass for staleness, correctness, secrets/privacy, shell-safety, and visual polish against the current web/product surface after main advanced with EPIC dispatch guards, a v1.2.558 changelog expansion, and macOS offline Retry polish.

## Bead(s)

- `bd-1d2e41` — Add technical-writer persistent agent profile for documentation freshness
- Audited landed work: `bd-9496d1` — EPIC/umbrella beads are coordinator records and worker-spawn surfaces reject them
- Audited landed work: `bd-c7ceeb` — macOS offline Retry in-flight progress and last-attempt feedback
- Audited landed work: `bd-0d4b79` — macOS offline Settings navigation feedback
- Audited release update: `v1.2.558`

## Before state

- Failing tests: none known in this docs-only checkout.
- Relevant metrics: main advanced from `efbb75c5` to `526d51fd`, touching `AGENTS.md`, `README.md`, `SPEC.md`, `CHANGELOG.md`, beads dispatch/claim code, macOS `RootView.swift`, and `scripts/macos-app-pane-navigation-smoke.sh`.
- Context: README/AGENTS/SPEC already mentioned the EPIC worker-dispatch guard, but the published Pages bead/CLI docs did not. macOS docs still described offline Retry/Settings generically, not the disabled Retry in-flight/progress/last-attempt behavior or the Settings acknowledgement/Cmd+comma/blocked-navigation timestamp behavior. The latest v1.2.558 changelog block still contained a collapsed `_…and 1 more in this bucket_` placeholder and concrete node wording.

## After state

- Failing tests: none observed.
- Relevant metrics: `./docs/validate-pages.sh` passed with 1414 checks, 0 warnings, 0 failures; `cargo run -p caco-profile --bin caco-docs-gen -- --check` reported `docs/profiles.html already up-to-date`; `git diff --check` passed; bash/sh command-block placeholder scan passed; top-level HTML duplicate-root, raw-Markdown-link, external tracker/CDN, and pre-command placeholder scan passed; docs/style.css brace and caco-web token parity spot-check passed; latest changelog placeholder/privacy scan passed.
- Context: Pages now explain that synchronous and queued worker dispatch reject EPIC/umbrella beads encoded by type, title prefix, or label. macOS docs now describe Retry progress, disabled state, last-attempt timestamp behavior, and the offline Settings acknowledgement/Cmd+comma/blocked-navigation timestamp behavior. The current changelog block now records `bd-c7ceeb` under Unreleased, expands the v1.2.558 collapsed macOS entry to `bd-97cda8`, and removes concrete node wording from the latest release block.

## Diff summary

- Commits: `96898fdc`, `b7925492`
- Files touched: `CHANGELOG.md`, `companion/macos/README.md`, `docs/beads.html`, `docs/cli.html`, `docs/macos-development.html`, `docs/macos-development.md`.
- Tests: +0 / -0 / flipped 0; validation was static Pages QA, profile-doc generator check, whitespace check, shell-placeholder scans, top-level HTML privacy/link/CDN scans, latest changelog placeholder/privacy scan, and CSS brace/token parity spot-check.
- Behavioural delta: documentation-only. No application logic, tests, workflow behavior, build configuration, or generated profile docs changed.

## Operator-takeaway

The Pages site now matches the latest bead-dispatch and macOS offline Retry/Settings behavior, and the current changelog block no longer carries a collapsed placeholder or concrete node wording.

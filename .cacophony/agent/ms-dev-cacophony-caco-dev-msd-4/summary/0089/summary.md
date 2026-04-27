# Session summary — macOS durable QA root and profile-doc drift

## Goal

Make the macOS `Cacophony Test.app` and `Cacophony Canary.app` default install root durable across shell sessions, while also clearing a separate broken-on-main profile-doc regeneration failure discovered during validation.

## Bead(s)

- `bd-c10ad8` — Use a stable default QA root for macOS Test and Canary apps
- `bd-10cc24` — [broken-on-main] tests::shipped_profiles_html_matches_autogen_output failing

## Before state

- Failing tests: `cargo test-small` failed in `tests::shipped_profiles_html_matches_autogen_output` because `docs/profiles.html` was stale against `.cacophony/profiles/`.
- Relevant metrics: `scripts/macos-app-qa-launch.sh --print-paths` could default Test/Canary paths under a transient nix-shell `TMPDIR`, e.g. `/var/folders/.../nix-shell.../cacophony-macos-qa`.
- Context: the macOS cloud refresh path already fed artifacts into the QA launcher, but the launcher's default root was less stable than the operator contract for persistent Test/Canary identities.

## After state

- Failing tests: none in validation.
- Relevant metrics: the default QA root is now `~/.cacophony/macos-qa` via `DEFAULT_QA_ROOT`, while `CACO_MACOS_QA_ROOT`, `CACO_MACOS_TEST_APP`, `CACO_MACOS_QA_APP`, and `CACO_MACOS_CANARY_APP_PATH` continue to override paths.
- Context: provenance inspection now uses the same durable Test/Canary default root, source smokes assert the old transient TMPDIR fallback is absent, and `docs/profiles.html` is regenerated separately for the caco-web profile mode drift.

## Diff summary

- Commits: `44b418912`, `d9cf874cd`
- Files touched: `scripts/macos-app-qa-launch.sh`, `scripts/macos-app-provenance.sh`, `scripts/macos-app-pane-navigation-smoke.sh`, `scripts/macos-app-cloud-refresh-smoke.sh`, `README.md`, `AGENTS.md`, `SPEC.md`, `docs/macos-development.md`, `docs/macos-development.html`, `companion/macos/README.md`, `docs/profiles.html`
- Tests: `bash -n scripts/macos-app-qa-launch.sh scripts/macos-app-provenance.sh scripts/macos-app-cloud-refresh-smoke.sh scripts/macos-app-pane-navigation-smoke.sh`; `scripts/macos-app-qa-launch.sh --print-paths`; `scripts/macos-app-cloud-refresh-smoke.sh`; `scripts/macos-app-pane-navigation-smoke.sh`; `docs/validate-pages.sh`; `just docs-build`; `cargo test-small`.
- Behavioural delta: Test/Canary app refreshes now land by default in a persistent per-user directory instead of a shell temp directory, and the profile docs autogen table is back in sync.

## Operator-takeaway

The native macOS QA app identities now survive shell restarts by default, and the unrelated profile-doc drift is explicitly tracked and fixed as broken-on-main rather than being folded into the macOS feature.

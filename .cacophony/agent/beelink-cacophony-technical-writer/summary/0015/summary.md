# Session summary — Pages audit after TTS JSON envelope changes

## Goal

Run another full GitHub Pages pass for staleness, correctness, secrets/privacy, and visual polish against the current caco-web surface after new Android polish, AKS-lite docs, release, and TTS JSON-envelope commits landed.

## Bead(s)

- `bd-1d2e41` — Add technical-writer persistent agent profile for documentation freshness

## Before state

- Failing tests: none known.
- Relevant metrics: recent commits changed `caco tts status --json` and `caco tts io output show --json` to return the standard `{ok,data,meta}` envelope, added stronger Pages staging validation, and touched Android/macOS/AKS surfaces. The Pages validator was expected to pass, but the CLI/README TTS docs did not yet describe the new JSON envelope behavior.
- Context: operator/router messages said recent ms-mac TTS probes were landing, so this pass did not emit extra TTS probes and focused on documentation correctness.

## After state

- Failing tests: none observed.
- Relevant metrics: `./docs/validate-pages.sh` passed with 1374 checks, 0 warnings, 0 failures; `cargo run -p caco-profile --bin caco-docs-gen -- --check` reported `docs/profiles.html already up-to-date`; `git diff --check` passed. CSS spot-check confirmed docs and caco-web still share key Nord palette, semantic surfaces, font stacks, radius, and transition values.
- Context: `README.md` and `docs/cli.html` now document the TTS JSON envelope behavior. README TTS examples also avoid shell-hostile placeholders and fix an inline environment-assignment example that would not have expanded `AGENT_ID` correctly in a POSIX shell.

## Diff summary

- Commits: `fdce3f4e`
- Files touched: `README.md`, `docs/cli.html`.
- Tests: +0 / -0 / flipped 0; validation was static Pages QA, profile-doc generation check, whitespace check, focused safety/privacy scans, visual token spot-check, and manual recent-commit review.
- Behavioural delta: documentation-only. No application logic, workflows, tests, or binary assets changed.

## Operator-takeaway

The Pages site remains visually aligned with caco-web and passes the stricter staging validator. The only fresh drift found was TTS CLI JSON output shape, now documented so scripts know `tts status --json` and `tts io output show --json` can be checked through `.ok` like the rest of the standard envelope surfaces.

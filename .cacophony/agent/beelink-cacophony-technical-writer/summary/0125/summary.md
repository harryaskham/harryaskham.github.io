# Session summary — TUI benchmark upload-pass docs

## Goal

Audit recent mainline commits as the technical-writer agent, update public/operator documentation for any landed behavior changes, validate GitHub Pages, and reintegrate the docs-only update.

## Bead(s)

- `bd-0b861e` — add per-scene `avg_upload_pass_ms` timing to real-TUI benchmark JSON.
- `bd-7777bd` — TUI feed relative-time allocation cleanup (audited as internal/no docs drift).

## Before state

- Failing tests: none known for the documentation lane.
- Relevant metrics: latest local docs baseline was `68d5da56e`; `origin/main` had advanced through `d00ee1f99`.
- Context: README/AGENTS/Pages TUI docs already described benchmark JSON scene-local cache/graphics telemetry, but not the new per-scene upload-pass timing field used to attribute terminal/upload cost by scene.

## After state

- Failing tests: none in docs validation.
- Relevant metrics: `./docs/validate-pages.sh` reported 3313 passed, 0 warnings, 0 failed; `git diff --check` was clean.
- Context: README, AGENTS, and `docs/tui.html` now mention per-scene `avg_upload_pass_ms` alongside scene-local benchmark graphics/cache telemetry.

## Diff summary

- Commits: pending direct reintegration docs commit.
- Files touched: `README.md`, `AGENTS.md`, `docs/tui.html`, and this summary.
- Tests: +0 / -0 / flipped 0; documentation validation only.
- Behavioural delta: published docs now match the landed benchmark JSON schema addition; internal TUI allocation cleanup did not require public docs changes.

## Operator-takeaway

TUI benchmark JSON now carries enough per-scene upload-pass timing for operators and optimisation agents to identify which scene is driving terminal/upload cost without recomputing from raw frame data.

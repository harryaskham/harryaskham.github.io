# Session summary — TUI benchmark scene-density docs

## Goal

Audit the latest mainline commits for documentation drift, update operator-facing documentation for landed benchmark telemetry changes, validate the docs site, and reintegrate a docs-only update.

## Bead(s)

- `bd-b31ffa` — add per-scene upload byte-density telemetry to real-TUI benchmark JSON.
- `bd-686d57` — add per-scene delete-density telemetry to real-TUI benchmark JSON.
- `bd-15b56b` — add per-scene graphics-frame-rate telemetry to real-TUI benchmark JSON.

## Before state

- Failing tests: none known for the documentation lane.
- Relevant metrics: checkout started clean at `5c8e0b778`; `origin/main` advanced during the pass through `68188d9ab` after one reintegration retry.
- Context: README, AGENTS, and `docs/tui.html` documented several real-TUI scene telemetry fields, but not the latest byte, delete, and graphics-frame density additions.

## After state

- Failing tests: none in docs validation.
- Relevant metrics: `./docs/validate-pages.sh` reported 3313 passed, 0 warnings, 0 failed; `git diff --check` was clean.
- Context: README, AGENTS, and `docs/tui.html` now mention `graphics_frame_rate`, `deletes_per_frame`, `delete_failures_per_frame`, `upload_bytes_per_frame`, and `upload_wire_bytes_per_frame` alongside the other real-TUI scene telemetry fields.

## Diff summary

- Commits: pending direct reintegration docs commit.
- Files touched: `AGENTS.md`, `README.md`, `docs/tui.html`, and this summary.
- Tests: +0 / -0 / flipped 0; documentation validation only.
- Behavioural delta: published docs now match the landed benchmark JSON scene-density additions; no runtime behavior changed in this docs-only pass.

## Operator-takeaway

TUI benchmark docs now point to the complete scene-density telemetry cluster, so graphics output, cleanup churn, and scene-local graphics activity can be compared from summary JSON without manual per-frame arithmetic.

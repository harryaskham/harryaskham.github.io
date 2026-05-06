# Session summary — service manual-control runbook

## Goal

Document a safe first-party manual-control mode for local Cacophony services after the 2026-05-06 outage, so operators can pause native supervision, inspect and start only requested services, and restore supervision without causing duplicate listeners or broad process kills.

## Bead(s)

- `bd-5d734f` — Document and automate a safe manual-control mode for local Cacophony services

## Before state

- Failing tests: none known for this documentation/config-helper slice.
- Relevant metrics: `caco config validate --project-config-dir=$(pwd)/.cacophony` passed before editing; checkout was clean and assigned to `bd-5d734f`.
- Context: README and SPEC already described `caco service disable` as a manual-startup escape hatch, but the operator-facing restart-window guide did not provide a step-by-step runbook covering one-owner coordination, first-party disable/load, manual PID/listener inspection, and caco-web port 11180 ownership warnings.

## After state

- Failing tests: none observed.
- Relevant metrics: `caco config validate --project-config-dir=$(pwd)/.cacophony` passes; `docs/validate-pages.sh` passes; `docs/sibling-update.sh --check-only` reports paired Markdown/HTML siblings in sync; `git diff --check` passes; read-only checks `caco service status --json`, `caco web status --json`, and `caco ps --kind service --json` returned structured service/supervisor status.
- Context: `docs/controller-restart-windows.md` and its styled HTML sibling now include a Safe Manual-Control Mode runbook. README and AGENTS now point operators/contributors at the same safe manual-control shape.

## Diff summary

- Commits: documentation commit plus this summary commit; exact SHAs may be rewritten by first-party rebase before reintegration.
- Files touched: `AGENTS.md`, `README.md`, `docs/controller-restart-windows.md`, `docs/controller-restart-windows.html`, `.cacophony/agent/ms-mac-cacophony-config-helper/summary/pending/summary.md`.
- Tests: +0 / -0 / flipped 0; documentation/config validation only.
- Behavioural delta: no runtime code changed. The documented operator path now requires one controller owner, `caco service disable --json`, status verification, service-family stops instead of broad pkill, PID/listener inspection through first-party status surfaces, explicit caco-web 11180 ownership checks, and deliberate restoration with `caco service load --json` plus `caco up --skip-update --json`.

## Operator-takeaway

The manual-control guidance is now concrete enough for the next incident: it tells operators how to pause and restore the native supervisor safely, how to see active manual service owners, and how to avoid the duplicate caco-web/listener and broad-kill hazards that made the outage risky.

# Session summary — Codespaces per-task secret decision

## Goal

Resolve the documentation ambiguity around whether `caco codespace secret push/list/remove` is a shipped feature or a future design. The goal was to make the current operator guidance match the implemented CLI surface without starting new secret-management implementation work.

## Bead(s)

- `bd-eadb2b` — [docs] Decide whether Codespaces per-task secret push should ship

## Before state

- Failing tests: none; this was a documentation/product-scope task.
- Relevant metrics: `crates/caco-cli/src/lib.rs` registers `caco codespace new/ls/stop/resume/revoke/remove/rekey/enroll`, but no `caco codespace secret push/list/remove` family.
- Context: `docs/codespaces.md` already warned that per-task secret commands were not shipped, while the key-distribution design still read like a future CLI contract without an explicit deferral decision.

## After state

- Failing tests: none.
- Relevant metrics: `git diff --check` passed; targeted grep confirms `bd-eadb2b` decision language appears in both `docs/codespaces.md` and the key-distribution design.
- Context: docs now explicitly decide to keep first-party Codespaces per-task secret commands out of the current slice and point operators to GitHub Codespaces user secrets or repo-owned container secret-file projection.

## Diff summary

- Commits: `3467916a2` (docs), plus the recorded-summary commit containing this file.
- Files touched: `docs/codespaces.md`, `docs/epics/bd-f32dda-codespaces-key-distribution.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-1/summary/0003/summary.md`.
- Tests: +0 / -0 / flipped 0.
- Behavioural delta: no CLI behavior changed; operator docs no longer imply the per-task secret command family is imminent or shipped.

## Operator-takeaway

Codespaces per-task secrets are intentionally deferred for now. Operators should continue using GitHub Codespaces user secrets for native devcontainers or the `deploy/codespaces/runtime/secrets/` file projection for the repo-owned container flow until a future implementation bead is explicitly prioritized.

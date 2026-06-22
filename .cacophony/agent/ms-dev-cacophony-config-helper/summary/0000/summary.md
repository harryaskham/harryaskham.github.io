# Session summary — Re-arm the reintegration fast-test gate for caco-tui & caco-web

## Goal

Close an ungated-reintegration reliability gap: the `caco-tui` and `caco-web`
persistent Rust dev agents were landing crate changes to `main` with only a
rebase-freshness check, because their cacophony value-level declarations did not
compose the `cacophony-fast-tests` before-reintegration gate. The goal was a
narrow, correctly-scoped config change so those agents' reintegrations run
`cargo check --workspace --tests`, `cargo test-small`, and `cargo clippy`
automatically, without per-land manual compensation, and without leaking the
cacophony-only gate into other projects.

## Bead(s)

- `bd-463493` — caco-tui (+ likely caco-web) Rust dev reintegrations are UNGATED
  — value level omits cacophony-fast-tests, risks ungated Rust lands (bug, P2;
  filed by caco-ctrl, discovered via bd-7c03ee routing).

## Before state

- Failing tests: none (this is a config-composition gap, not a test failure).
- `.cacophony/agents/cacophony_persistent.yaml` value defs:
  - `caco-tui` → `profile: [caco-tui]` (no gate)
  - `caco-web` → `profile: [caco-web]` (no gate)
- Only `values.caco-dev` / `values.caco-dev-codex` composed `cacophony-fast-tests`,
  so the caco-dev-* declarations were gated but caco-tui / caco-web were not.
- `caco-tui.md` / `caco-web.md` set a `reintegration:` (mode) block but no
  `reintegration_checks`, so the CLI-side before_reintegration fast-test gate ran
  no checks for them; caco-tui was compensating manually per land.
- `caco config validate`: clean, 12 nodes, 20 projects.

## After state

- Failing tests: none.
- `caco-tui` → `profile: [caco-tui, cacophony-fast-tests]`
- `caco-web` → `profile: [caco-web, cacophony-fast-tests]`
- `caco profile compose --dry-run caco-tui cacophony-fast-tests` →
  `effective profile: caco-tui+cacophony-fast-tests`, must-agree-conflicts 0.
- `caco config validate --project-config-dir`: clean, 12 nodes, 20 projects.
- Gate stays cacophony-scoped: composed only at the cacophony value level in
  `cacophony_persistent.yaml`, never in the shared `cacophony_dev.yaml` /
  `codex-dev.yaml` base (bd-836eb4 leak guard preserved).

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Summary artefact commit: intentionally omitted (must not self-reference).
- Files touched: `.cacophony/agents/cacophony_persistent.yaml` (added
  `cacophony-fast-tests` to the `caco-tui` and `caco-web` value-level profile
  lists, with bd-463493/bd-836eb4 rationale comments).
- Tests: +0 / -0 / flipped 0 (config-only).
- Behavioural delta: after this lands and the caco-tui / caco-web agents are
  refreshed/recreated, their reintegrations run the cacophony-fast-tests gate
  (check/test-small/clippy, abort_on_failure) instead of rebase-freshness only.

## Operator-takeaway

caco-tui and caco-web are now gated on reintegration like the caco-dev-* workers,
closing the bd-5d8c78 ungated-Rust-land class for those two surfaces. The fix is
config-only and value-level-scoped, so it does not leak the gate into other
projects. Live caco-tui / caco-web agents will only pick up the gate after a
profile refresh/recreate (source change alone does not update a running
persistent agent in place) — a controller/operator refresh is the deployment
step. caco-android / caco-ios / caco-macos land Kotlin/Swift and are correctly
out of scope.

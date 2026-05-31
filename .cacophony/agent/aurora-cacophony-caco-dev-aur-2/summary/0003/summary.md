# Session summary — Gate direct reintegration on test-small (bd-0b1572)

## Goal

Compose the existing `cacophony-fast-tests` before_reintegration hook
(cargo test-small + cargo check --workspace --tests + cargo clippy) into the
generic Rust dev/worker profile stacks so behaviour-shifting landings can no
longer accumulate stale-sibling test failures on main unseen. Operator-approved
(Harry) and directly motivated by this session's own incident, where a behaviour
change landed and broke sibling tests that a test-small gate would have caught.

## Bead(s)

- `bd-0b1572` — Gate direct reintegration on test-small: compose cacophony-fast-tests into direct-reintegration dev/worker profile stacks

## Before state

- `cacophony-fast-tests` existed but was only referenced by `merge-queue.md`
  (prose only — no hook) and `profiles.yaml`. The generic dev/worker stacks
  (`cacophony_dev.yaml` = dev + auto-claim; `codex-dev.yaml`) did direct
  reintegration with NO real test gate.
- Verified test-small was green on main first (fixed the last red lane,
  bd-b448cc) so enabling a fleet-wide gate would not wedge reintegration.

## After state

- `cacophony-fast-tests` composed into `.cacophony/agents/cacophony_dev.yaml`
  and `.cacophony/agents/codex-dev.yaml` — the generic dev/auto-claim worker
  bases imported by `dev.yaml` and the `caco-dev-*` declarations.
- Placed on the dev/worker base (not `persistent.yaml`/`base.yaml`) so role
  specialists (caco-web/macos/android), controllers, role agents, and
  persistent-observers are NOT forced to run cargo test-small; the low-power
  `light-dev` stack stays intentionally exempt.
- `caco config validate` passes; `caco profile compose --dry-run` for the full
  dev stack resolves with zero must-agree conflicts.
- AGENTS.md and README.md updated per the Documentation Contract.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- Files touched: `.cacophony/agents/cacophony_dev.yaml`,
  `.cacophony/agents/codex-dev.yaml`, `AGENTS.md`, `README.md`.
- Tests: none (config/docs change); validated via config validate + profile compose.
- Behavioural delta: future dev/worker direct reintegrations now run the
  test-small/check/clippy gate before landing.

## Operator-takeaway

Direct reintegration for the generic Rust dev/worker stacks is now gated on
test-small, closing the gap that let behaviour-shifting landings accumulate
stale-sibling test failures on main. Specialists, controllers, observers, and
low-power light-dev workers are deliberately exempt so the gate only runs where
the Rust test-small lane is meaningful. merge-queue remains prose/queue-routing
only, so this is the first real gate rather than a double-gate.

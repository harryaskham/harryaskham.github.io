# Session summary — bd-acfa92 runtime launch timeout env override

## Goal

Make DEFAULT_RUNTIME_LAUNCH_TIMEOUT_SECS / NODEJS_RUNTIME_LAUNCH_TIMEOUT_SECS
operator-tunable without a daemon recompile.

## Bead(s)

- `bd-acfa92` — DEFAULT_RUNTIME_LAUNCH_TIMEOUT_SECS magic constant

## Before state

- Hardcoded constants (2s default, 10s for codex/pi)
- Operators hitting flaky launch verification on slow/loaded hosts
  had no knob short of recompile

## After state

- Env-var override `CACO_RUNTIME_LAUNCH_TIMEOUT_SECS_<TYPE>` taken
  per agent_type; bit-for-bit fallback to the canonical constants
  when unset/unparseable/zero/negative.
- 2 new tests guarding override + fallthrough.
- Full config plumbing through OperatorConfig deferred as follow-up;
  env-var path is a standard caco escape hatch.

## Diff summary

- Commits: de61a9897ebe
- Files: `crates/caco-daemon/src/agent/mod.rs`,
  `crates/caco-daemon/src/agent/tests.rs`
- Tests: +2

## Operator-takeaway

Same pattern is broadly applicable to any other "magic constant
discovered to be wrong by hindsight" — env var first, config plumbing
when the schema work is justified. For now operators can set
`CACO_RUNTIME_LAUNCH_TIMEOUT_SECS_PI=15` to widen the window for
slow hosts.

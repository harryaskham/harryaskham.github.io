# Session summary — bd-368c7f rewind successor spawn invocation

## Goal

Address `bd-368c7f`: invoke/prepare the canonical managed spawn surface with assembled rewind-successor context after context assembly exists. Bead ownership handoff remains out of scope.

## Changes

- Added `RewindSuccessorManagedSpawnInvocation`.
- Added `build_rewind_successor_managed_spawn_invocation(...)` to convert assembled rewind context into canonical managed-spawn request fields:
  - project/node/agent type/profile
  - bead id
  - bounded goal text
  - isolated checkout path
  - `CACO_REWIND_SUCCESSOR_CONTEXT_JSON` environment envelope
- Added refusal for missing required project/node/agent type values.
- Added regression covering context propagation and refusal behavior.

## Validation

- `cargo test -p caco-daemon --lib build_rewind_successor_managed_spawn_invocation_carries_context_bd_368c7f -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `bbdc21de2f`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.

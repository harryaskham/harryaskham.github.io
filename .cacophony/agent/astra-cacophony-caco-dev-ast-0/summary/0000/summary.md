# Session summary — bd-b89375 lifecycle-operations API failure/edge integration tests

## Goal

Harden the daemon's lifecycle-operations HTTP API by adding integration
coverage for its failure and edge modes. Before this session the suite only
exercised the create/poll/events happy path, so a refactor that broke intent
validation, not-found handling, or auth could silently regress the release
surface. This chunk locks in the status codes and error envelopes for the
unhappy paths.

## Bead(s)

- `bd-b89375` — Harden lifecycle-operations API with integration tests for
  failure/edge modes (unknown-intent 400, not-found 404, auth)

## Before state

- Failing tests: none
- Lifecycle-operations API integration coverage: happy path only
  (`lifecycle_operation_api_create_poll_and_events_bd_2a8d8c`)
- Context: recovered from an involuntary socket-death stop; the in-flight work
  was preserved in a WIP checkpoint that also bundled an unrelated
  `validate.rs` fixture cleanup.

## After state

- Failing tests: none expected (the merge-queue gate runs
  `cargo check --workspace --tests` + `cargo test-small` + clippy on the merge
  commit; this phone node defers heavyweight local validation to that gate).
- Lifecycle-operations API integration coverage: happy path + 6 new
  failure/edge tests.
- Context: branch is a single focused commit, rebased onto current main,
  working tree clean.

## Diff summary

- Code commit: `aefcc29d0` (final landed squash SHA comes from the
  reintegration receipt).
- Summary artefact commit: intentionally omitted (must not self-reference its
  own mutable SHA).
- Files touched: `crates/caco-daemon/src/lib.rs` (+173).
- Tests: +6 (all `*_bd_b89375`):
  - `lifecycle_operation_api_unknown_intent_rejected` — agent-scoped POST
    unknown intent -> 400 `invalid_lifecycle_operation_intent`
  - `lifecycle_operation_api_project_unknown_intent_rejected` — project-scoped
    POST unknown intent -> 400 `invalid_lifecycle_operation_intent`
  - `lifecycle_operation_api_project_rejects_existing_agent_intent` —
    project create against an existing agent is rejected
  - `lifecycle_operation_api_show_not_found` — show missing op -> 404
    `lifecycle_operation_not_found`
  - `lifecycle_operation_api_events_not_found` — events for missing op -> 404
    `lifecycle_operation_not_found`
  - `lifecycle_operation_api_requires_auth` — missing/invalid bearer -> 401
    `unauthorized`
- Behavioural delta: none (test-only). Out-of-scope `validate.rs`
  `..Default::default()` cleanup that the WIP checkpoint had bundled was
  dropped to keep the bead focused and avoid conflict surface on a hot file.

## Operator-takeaway

The lifecycle-operations API now has regression guards on its unhappy paths
(400 unknown-intent, 404 not-found, 401 auth), not just the happy path, so the
release surface is protected against silent error-handling drift. The tests
follow the existing `test_state` / `local_router` harness, so they cost almost
nothing to maintain. Note for infra: the involuntary-stop WIP checkpoint
bundled an unrelated cross-crate cleanup into the preserved work, which had to
be manually un-bundled — worth keeping an eye on as a recurring friction shape.

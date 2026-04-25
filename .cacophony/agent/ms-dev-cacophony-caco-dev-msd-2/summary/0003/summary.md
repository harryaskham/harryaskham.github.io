# Session summary — short-name routing ambiguity detection (bd-4dd73a)

## Goal

Audit and fix agent short-name routing in `caco agent --name` CLI
surface. Operator reported surprise ("ah the short names are being
routed?!") when `msm-4` resolved silently to one of several persistent
agents sharing the same suffix (e.g. `caco-dev-msm-4`,
`caco-config-helper-msm-4`, `caco-cluster-debugger-msm-4`). The fix
makes ambiguous short-name lookups error explicitly with all matches
listed, instead of silently picking the first one.

## Bead(s)

- `bd-4dd73a` — Audit short-name routing for agent control surfaces.

## Before state

- `resolve_agent_id_by_name` matched `pid == name || pid.ends_with("-{name}")`.
  When multiple agents matched the suffix, it returned the first
  project-scoped hit (or first any-project hit) — silently routing
  operator commands to whichever agent happened to appear first in the
  daemon list. Zero ambiguity detection.
- CLI test suite had a pre-existing broken-on-main duplicate test
  definition (`dispatch_codespace_new_pushes_rendezvous_bootstrap_secret_bd_0bed93`
  defined twice) preventing `cargo test -p caco-cli --lib` from
  compiling.

## After state

- Factored out `resolve_agent_id_by_name_in_response(name, body,
  current_project)` — pure resolver taking the parsed `/api/v1/agents`
  envelope so the project-scoping + ambiguity logic is unit-testable
  without a live daemon.
- Match precedence: (1) exact `persistent_id` match (always
  unambiguous) → prefer current project, (2) suffix match scoped to
  current project → error if >1, (3) suffix match globally → error
  if >1. Every ambiguity path surfaces `bd-4dd73a:` cite, all
  matching `persistent_id`s, and suggests using the full ID or
  setting `CACO_PROJECT` to disambiguate.
- Removed the duplicate test definition (broken-on-main sidecar fix).
- 6 new tests:
  - `short_name_resolves_unambiguous_in_current_project`
  - `short_name_refuses_silent_routing_when_ambiguous_in_project` (core bug)
  - `short_name_exact_match_wins_over_suffix_collisions`
  - `short_name_project_scope_disambiguates_cross_project_collision`
  - `short_name_global_ambiguity_suggests_caco_project`
  - `short_name_no_match_returns_not_found_error`
- `cargo build -p caco-cli`: clean. All 6 new tests pass.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - Refactored `resolve_agent_id_by_name` into thin network wrapper +
    `resolve_agent_id_by_name_in_response` pure resolver (~120 LOC).
  - Removed duplicate `dispatch_codespace_new_...` test definition
    (broken-on-main sidecar).
  - +6 tests (~100 LOC) with `agent_envelope` test helper.

## Operator-takeaway

The daemon-side `AgentManager::resolve_agent_id` (lifecycle.rs:2720)
has a similar first-match-wins pattern using `agent_name` suffix, but
it's used for internal routing where the caller provides a full
composite ID (not operator-supplied short names). If that surface also
needs ambiguity detection, it's a separate bead with its own test
pattern. This PR covers only the CLI-facing `--name` flag used by
operators (e.g. `caco agent nudge --name msm-4`).

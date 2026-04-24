# Session 0017 — bd-cf54cb (RE-IMPLEMENTATION)

## What

**bd-cf54cb** was closed earlier (~12h ago per bead metadata) but the
implementation never reached origin/main — verified by grepping
`origin/main:crates/caco-daemon/src/reintegration.rs` and finding zero
`pub state_branch:` fields, and zero `resolve_cacophony_state` callsites
in caco-cli/caco-daemon lib on main. This is a bd-c0b499-class
silent-loss: bead closed but squash never landed.

Per operator directive (24.04): reopened bd-cf54cb with `caco bd update
--status open`, added an append-description note explaining the
premature close, re-claimed, re-implemented from scratch, rebased on
fresh main (which added bd-ab3050 refuse-on-dirty preflight and a JSON
envelope wrap on `caco agent artefacts`), and shipped via `caco agent
ship` this time.

## Changes

`crates/caco-daemon/src/cacophony_state.rs`
- Added `state_branch: &str` param to six public functions:
  `remote_branch_exists`, `resolve_state_branch_tip`,
  `split_and_commit_artefacts`, `push_both_branches_atomic`,
  `push_cacophony_state`, `list_agent_artefacts`.
- All internal `CACOPHONY_STATE_BRANCH` const uses replaced with the
  threaded param.
- Four existing in-file tests updated to pass the default const.
- New integration test `split_honours_per_project_state_branch_override`:
  project with prefix `picasso/cacophony-state` produces (a) override
  branch created locally, (b) default `cacophony-state` NOT created,
  (c) `push_cacophony_state` lands override on bare remote, (d)
  `list_agent_artefacts` enumerates artefacts on override branch.

`crates/caco-daemon/src/reintegration.rs`
- Added `pub state_branch: String` field to `ReintegrationRequest`.
- Threaded through `finalize_direct_merge` and
  `finalize_direct_merge_with_checkout_recovery` signatures to the
  three cacophony_state callsites (split, push-both-atomic, push-state).
- All 50 `ReintegrationRequest { ... }` test fixtures updated to pass
  `CACOPHONY_STATE_BRANCH.to_string()` (preserves bit-for-bit
  behaviour).

`crates/caco-daemon/src/test_bridge.rs`
- `ReintegrationRequest` construction at line 149 passes
  `CACOPHONY_STATE_BRANCH.to_string()`.

`crates/caco-cli/src/lib.rs`
- Two `dispatch_reintegrate` variants (single-project and
  cross-project) resolve per-project override:
  `proj.and_then(|p| p.branches.as_ref())
      .map(|b| b.resolve_cacophony_state(CACOPHONY_STATE_BRANCH))
      .unwrap_or_else(|| CACOPHONY_STATE_BRANCH.to_string())`.
- `dispatch_agent_artefacts` does the same resolve + passes to
  `list_agent_artefacts` and reports the resolved branch name in
  both JSON and text output.
- Merged cleanly with main's bd-ab3050 refuse-on-dirty preflight and
  the envelope-wrapping of `caco agent artefacts --json`.

## Verification

- `cargo test -p caco-daemon --lib reintegration` → 136 pass.
- `cargo test -p caco-daemon --lib cacophony_state` → 7 pass (includes
  new override test).
- `cargo clippy -p caco-daemon -p caco-cli --no-deps --tests` → clean;
  only pre-existing `too_many_arguments` and `doc_lazy_continuation`
  warnings on unrelated code.
- Default behaviour preserved bit-for-bit when `proj.branches.is_none()`.

## Close-discipline notes

- Reopened bd-cf54cb via `caco bd update --status open` + appended a
  REOPENED note with timestamp and the origin/main grep evidence before
  re-claiming.
- This session also audited my claimed beads: all 4 in_progress P3/P4s
  in my inbox are CLI-UX triage beads claimed by other agents (msd-1,
  msd-4, msd-5, wmi-1, wmi-2); not my work.
- Answered operator lost-work check via `caco msg speak`: 0 commits
  ahead of origin/main, no failed reintegrates since 00:00 UTC, daemon
  reachable, all recent closes traceable on origin/main except this
  bd-cf54cb which was the silent-loss I'm now re-landing.

## Next

- Cycle to next ms-mac-friendly P1/P2 bead after ship + sync.
- NOTE workspace-view do-over epic bd-5bfb2c is now PERMANENT + P0;
  operator rule = one agent/pair claims the WHOLE do-over as a coherent
  arc, no piecemeal. Do not pick workspace sub-beads.

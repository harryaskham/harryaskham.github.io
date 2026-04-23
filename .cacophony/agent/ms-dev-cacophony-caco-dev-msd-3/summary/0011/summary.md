# Session summary — bd-5bae2e: thread default_branch through spawn

## Goal

Stop `create_shared_clone` from hardcoding `origin/main` as the
upstream for newly-created agent / manual checkout branches.
Projects whose default_branch isn't literally `main` (e.g.
picasso-health uses `harryaskham/health/main`) hit a bd-fbe1d7
warning on every spawn because origin/main doesn't exist on
their remotes; bare `git pull --rebase` and `git push` then
broke until the operator manually fixed the upstream.

## Bead(s)

- `bd-5bae2e` — create_shared_clone hardcodes origin/main as
  upstream — breaks projects with non-main default_branch

## Before state

- `create_shared_clone` ran
  `git branch --set-upstream-to=origin/main <branch>`
  unconditionally.
- `AgentCreateRequest` had no `default_branch` field; the spawn
  path had no way to know the project's mainline name.
- `reintegration.rs` was already correct: the CLI threads
  `proj.default_branch` into `ReintegrationRequest.target_branch`
  in `agent_complete` (line ~36399).

## After state

- New `create_shared_clone_with_default_branch(default_branch:
  &str, …)` resolves the upstream as
  `origin/<default_branch>`. Empty string falls back to `main`
  for backwards compatibility.
- `create_shared_clone` is now a thin wrapper that delegates
  with `"main"` so historic callers keep working bit-for-bit.
- `AgentCreateRequest::default_branch: Option<String>` threaded
  from project config in `lib.rs` (two construction sites) and
  `modes.rs` (one construction site).
- `agent/lifecycle.rs::create` reads `req.default_branch` and
  calls the new variant.
- 41 test-site struct literals get `default_branch: None`
  (sed sweep). Removed 22 accidental duplicates where the sed
  injected a second `default_branch: None` into structs (mostly
  `caco_config::Project` literals) that already had one at the
  top: beads.rs (9), checkout.rs (8), config_reload.rs (1),
  ui_stream.rs (1), lib.rs (3).

## Diff summary

- `crates/caco-daemon/src/agent/spawn.rs` (+96/-19): split into
  wrapper + new function; +2 tests using actual
  `harryaskham/health/main` branch name.
- `crates/caco-daemon/src/agent/types.rs` (+10): new field.
- `crates/caco-daemon/src/agent/lifecycle.rs` (+5/-1): wire-through.
- `crates/caco-daemon/src/lib.rs` (+12): two construction-site
  threads + duplicate cleanup.
- `crates/caco-daemon/src/modes.rs` (+5): construction-site thread.
- `crates/caco-daemon/src/agent/tests.rs`,
  `crates/caco-daemon/src/test_bridge.rs`,
  `crates/caco-daemon/src/spawn_routing.rs`: 41 test struct-literal
  updates.
- All 19 `agent::tests::create_*` tests pass; 3 spawn::tests::
  create_shared_clone_* tests pass; cargo build --workspace clean;
  cargo clippy -p caco-daemon clean.

## Embedded artefacts

(none)

## Operator-takeaway

The bug was a textbook hardcoded-string footgun: a defensive
upstream-set that worked for everyone using `main` and silently
degraded for everyone else. The `_with_default_branch` variant
+ wrapper pattern keeps the blast radius small (existing
callers unchanged) while letting fresh callers thread the
correct value. Future follow-up: the ~30 test-internal
`origin/main` literals in reintegration.rs are scoped to test
fixtures that build their own `main` branch — leaving them
hardcoded matches the surrounding test-data contract, so no
production paths remain hardcoded after this fix.

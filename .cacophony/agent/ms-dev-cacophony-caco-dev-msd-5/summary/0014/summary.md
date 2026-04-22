# Session summary — bd-e93a29: caco-procguard extraction

## Goal

Make the daemon's `child_wait` zombie-reaper guard available to
`caco-beads` (which runs sync git children but cannot depend on
`caco-daemon` without forming a cycle), so synchronous git spawns
register a protected window the daemon's `waitpid(-1, WNOHANG)`
sweep observes and skips. Resolves the root cause behind bd-180c6d's
defensive ECHILD-synthesis paths.

## Bead(s)

- `bd-e93a29` — `[bd-180c6d follow-up] expose child_wait::begin
   guard from a shared crate so caco-beads can register sync git
   children with the daemon zombie reaper`

## Before state

- `caco-daemon::child_wait::ACTIVE_ASYNC_CHILD_WAITS` +
  `begin_async_child_wait` lived inside `caco-daemon`. Five
  daemon-side call sites took the guard around tokio child waits.
- `caco-beads::store` ran sync git children via
  `std::process::Command` with no way to register a guard.
- `bd-180c6d` had to add defensive ECHILD synthesis in
  `collect_output_with_status` / `run_git_with_retry` to recover
  from races the reaper caused.

## After state

- New workspace crate `caco-procguard` (3 files, no transitive deps)
  hosts the canonical `ActiveChildWaitGuard` + `begin_child_wait` +
  `has_active_child_waits`.
- `caco-daemon::child_wait` becomes a thin re-export preserving the
  old API names (`begin_async_child_wait` / `has_active_async_child_waits`
  / `ActiveAsyncChildWaitGuard`); all five existing daemon call sites
  compile unchanged.
- `caco-beads` now depends on `caco-procguard` and wraps four
  production git-spawn paths in `store.rs` (`run_git`, `run_git_ok`,
  `has_any_remote_branch`, `find_donor_branch`) with
  `begin_child_wait`.

## Diff summary

- Commits: `cd6e1cbd`
- New crate: `crates/caco-procguard/{Cargo.toml,src/lib.rs}` (132
  lines incl. tests).
- Touched: `Cargo.toml` (workspace member),
  `crates/caco-daemon/{Cargo.toml,src/child_wait.rs}` (re-export),
  `crates/caco-beads/{Cargo.toml,src/store.rs}` (4 spawn-site
  guards + 1 linkage test).
- Tests: +3 (procguard nested-guard, daemon backwards-compat alias,
  beads procguard-counter linkage).
- Build + clippy clean on caco-procguard, caco-daemon, caco-beads;
  full workspace `cargo check --workspace --tests` green.

## Operator-takeaway

`caco-beads` synchronous git operations now register a protected
window the daemon zombie reaper observes and skips, removing the
race that bd-180c6d's defensive ECHILD-synthesis was working around.
The defensive paths remain as belt-and-suspenders for true fork-bomb
scenarios where the guard window is too narrow to matter. Future
workspace members that need to spawn sync children alongside the
daemon should depend on `caco-procguard` and wrap their spawn..wait
windows with `begin_child_wait()`.

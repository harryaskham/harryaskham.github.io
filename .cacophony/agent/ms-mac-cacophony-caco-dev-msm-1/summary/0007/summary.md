# Session summary — fix persistent_recreate test isolation

## Goal

Resolve bd-03a276: two `caco-daemon` lib tests
(`persistent_recreate_relaunches_project_controller_replacement` and
`running_persistent_agent_recreate_forces_destructive_relaunch`) were
failing intermittently with a tmux socket dial error followed by a 30s
handler timeout returning 500 instead of 200. Diagnose root cause and
land a fix without disturbing production code paths.

## Bead(s)

- `bd-03a276` — [broken-on-main] persistent_recreate tests fail with tmux
  socket dial error then 30s handler timeout
- (also closed earlier in session: `bd-4cbb82` — caco-web Dispatch button
  always 422s — fix already on main from prior session, leaked claim)

## Before state

- Failing tests in `cargo test -p caco-daemon --lib persistent`:
  - `persistent_recreate_relaunches_project_controller_replacement`
  - `running_persistent_agent_recreate_forces_destructive_relaunch`
- Failure shape:
  - `bd-7e2934: tmux set-environment -g PATH failed on socket
    'caco-agent-cacophony-localhost-cacophony-ctrl': error connecting to
    /private/tmp/tmux-501/caco-agent-cacophony-localhost-cacophony-ctrl
    (No such file or directory)`
  - `bd-3a85f7: handler timed out after 30s — POST
    /api/v1/persistent/<id>/recreate; returning 500`
  - `assertion left == right failed: left: 500, right: 200`
- Both tests passed in isolation but failed when other `persistent*`
  tests ran in parallel.

## After state

- Failing tests: none (in scope). `cargo test -p caco-daemon --lib
  persistent` is now 135/135 passing.
- Five other lib tests still failing on main, all confirmed pre-existing
  and unrelated:
  - `retention_sweep_skips_non_completed_agents` → filed `bd-578267`
  - Four `all_embedded_profile*` tests (filer.md frontmatter parse) →
    filed `bd-b4e52e`
- `cargo clippy -p caco-daemon --tests --no-deps` clean.

## Diff summary

- Commits: `b91cd037` — bd-03a276: parameterize persistent_recreate test
  fixture decl key
- Files touched: `crates/caco-daemon/src/lib.rs` (+14 / -2)
- Tests: 0 added / 0 removed; 2 fixed (no longer flaky under parallel
  execution).
- Behavioural delta: test-only. No production code changed. The
  `setup_persistent_recreate_test_fixture` helper now derives a unique
  persistent decl key from `(profile_name, std::process::id())` instead
  of hard-coding `"ctrl"`. This gives each test its own
  `agent_id` → its own per-agent tmux socket
  (`caco-agent-{project}-{agent_id}`) and its own tmux session name,
  eliminating the cross-test collision on the shared
  `caco-agent-cacophony-localhost-cacophony-ctrl` socket.

## Operator-takeaway

Per-agent tmux sockets + per-agent tmux session names are derived from
`{node}-{project}-{agent_name}`. When two parallel tests inject the same
persistent declaration name into the same `localhost_fixture` config,
they share a socket and a session — and one test's `tmux new-session`
sees a session of the same name already alive on the shared server,
blocking the alive-sentinel write past the 30s handler timeout. This is
a pattern worth remembering when adding new fixtures: persistent decl
keys MUST be parameterized (or include `std::process::id()`) the same
way temp dirs are, otherwise the failure mode is silent flake under
`cargo test` parallelism rather than a clean error. Two pre-existing
broken-on-main lib failures remain (bd-578267, bd-b4e52e) for follow-up.

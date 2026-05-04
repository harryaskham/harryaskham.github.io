# Session summary — tmux session creation settle window

## Goal

Implement a focused `bd-86a486` repair slice for recurring per-agent tmux socket/startup recovery failures after ms-mac restart/load windows.

## Bead(s)

- `bd-86a486` — Runtime repair reports per-agent tmux socket collapses after socket hardening

## Findings

Current live failures were no longer only the older per-agent socket-collapse counter. Several ms-mac persistent agents were failing at startup/resume with messages like:

- `tmux session creation failed ... exited immediately after creation`
- `alive sentinel was not written within 60s and tmux is not attachable`
- `tmux server on socket ... is dead: no server running`

The common path was `tmux new-session` returning success, followed by a very short post-create `has-session` verification window (~700ms total). Under ms-mac load/restart pressure, this can misclassify tmux settle/probe flakiness as immediate init/session death and then feed the retry/failure loop.

## Changes

- Updated `crates/caco-daemon/src/agent/health.rs`:
  - Added a shared bounded tmux session creation settle loop.
  - The launcher now waits up to 5 seconds after successful `tmux new-session` for `has-session` to become attachable.
  - The same settle helper is used for both legacy/default-socket and per-agent-socket session creation paths.
  - Failure diagnostics now say the session was not attachable within the bounded window and include the last tmux probe error, preserving evidence such as `connection refused` or `no server running`.
- Added regression coverage in `crates/caco-daemon/src/agent/tests.rs`:
  - `tmux_session_creation_waits_through_transient_probe_failures_bd_86a486`
  - Verifies persistent transient probe failures consume the full supplied settle window and include the final probe detail in diagnostics.
- Updated `SPEC.md` §16.4.5 to require bounded post-create tmux attachability verification and explicit probe diagnostics instead of immediate-exit classification.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-daemon/src/agent/health.rs crates/caco-daemon/src/agent/tests.rs` — passed.
- `git diff --check` — passed.
- `cargo check -p caco-daemon` — passed.
- `cargo clippy -p caco-daemon --lib --no-deps -- -D warnings` — passed.

## Known validation blocker

- Attempted focused daemon test: `cargo test -p caco-daemon tmux_session_creation_waits_through_transient_probe_failures_bd_86a486 -- --test-threads=1`.
- It did not reach the new test because the caco-daemon test target still fails to compile on the existing broken-on-main `Config { ... }` initializers missing the newer `macos` field. That is tracked by `bd-39f452` and currently owned outside this branch; I did not duplicate the mechanical initializer fix.

## Coordination

- I closed `bd-8a4683` first, then claimed `bd-86a486` only after the local strict board gate and `caco bd claim` succeeded.
- I spoke progress to the project during implementation.

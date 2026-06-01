# Session summary — bd-f26166 (named-socket liveness) + bd-e26ace (daemon test build)

## Goal

Stop tmux liveness detection from falsely reporting managed agents `failed` /
`tmux: unknown` when they run on a per-agent NAMED tmux socket. The false-dead
readings (ms-mac dev-2, 2026-06-01) triggered wasted recovery cycles, conflicting
`agent list` vs `agent status` surfaces, and risked discarding a healthy
in-flight agent. While validating, also fixed a separate broken-on-main daemon
test-build compile error that had been masking latent daemon test failures.

## Bead(s)

- `bd-f26166` — tmux liveness probes the DEFAULT socket, misses named-socket
  agent sessions (false failed/unknown). P2 bug, agent-lifecycle.
- `bd-e26ace` — [broken-on-main] caco-daemon test build fails: `LifecycleOperationError`
  not imported in store.rs (E0422). P1 bug, filed+fixed because it blocked
  bd-f26166's test validation (the daemon lib-test target could not compile).
- Filed (not fixed here): `bd-c3f0c5` — a pre-existing `reconcile_dead_tmux_takes_
  precedence_over_stall` failure that was hidden behind bd-e26ace's compile error;
  routed open to aur-2's idle agent-reconcile capacity.

## Before state

- Named-socket agents probed on the wrong tmux server -> false dead/unknown.
  Root cause: `AgentInfo.tmux_socket` has `#[serde(default = "current_tmux_socket_name")]`,
  so a record whose JSON OMITS the field deserializes to the daemon's OWN socket
  (e.g. bare `caco`), not the agent's `caco-agent-{project}-{id}` socket. Liveness
  call sites then probed that wrong socket.
- `cargo test -p caco-daemon` could not compile (E0422 in store.rs) — daemon lib
  tests were entirely un-runnable, masking latent failures.

## After state

- Failing tests: none introduced. `agent::tests::reconcile` = 57 passed / 1 failed,
  IDENTICAL to the store.rs-fix-only baseline (the 1 failure is pre-existing
  bd-c3f0c5, independent of this work — proven by stashing all liveness changes).
- New `AgentInfo::effective_tmux_socket()` resolves the real per-agent socket:
  preserves explicit `caco-agent-*` and isolated `caco-test-*` sockets, but
  repairs the unambiguous missing-field trap (empty, or the bare daemon default
  socket) to the deterministic `agent_tmux_socket_name(project, id)`. Wired into
  the liveness probe sites that read possibly-stale disk records: CLI
  `agent complete`/`status`/`doctor` (caco-cli), the daemon resume-outcome and
  Pi-refresh-reload-blocker probes (caco-daemon lib.rs), and persistent runtime
  liveness (persistent.rs, with a derive-from-project fallback). 3 new unit tests
  cover derive-from-default / preserve-explicit / repair-empty.
- store.rs: imported `LifecycleOperationError` test-locally (inside `mod tests`,
  not at file top) so the lib build carries no unused import under -D warnings.
- clippy `-p caco-daemon -p caco-cli --lib`: clean.

## Diff summary

- Code commits: pending final squash SHA from the reintegration receipt.
- Files touched: crates/caco-daemon/src/{agent/types.rs, lib.rs, persistent.rs,
  store.rs}, crates/caco-cli/src/lib.rs.
- Tests: +3 (effective_tmux_socket derive/preserve/repair-empty). The daemon
  test target now compiles again.
- Behavioural delta: liveness probes resolve the correct per-agent tmux socket
  for stale/missing-field records; no change for records that already store a
  real per-agent socket. The reconcile in-memory candidate path was deliberately
  left using the stored socket (launch-set, correct) to avoid disturbing the
  test socket model — the repair is scoped to disk-read/CLI/persistent paths
  where the missing-field trap actually occurs.

## Embedded artefacts

- none.

## Operator-takeaway

Two things worth remembering. First, the named-socket false-dead bug was a serde
default trap: a MISSING `tmux_socket` field silently became the daemon's own
socket, so the deterministic repair keys on "stored is empty or the bare daemon
default" rather than blindly deriving (which would have broken the test socket
model — an earlier over-broad version failed 41 reconcile tests before I scoped
it down). Second, bd-e26ace's one-line missing import had been hiding the entire
daemon lib-test target from compiling, which in turn hid at least one real
reconcile regression (bd-c3f0c5) from the gate — fixing the compile error is what
made that latent failure visible, so it's now filed and routed rather than lost.

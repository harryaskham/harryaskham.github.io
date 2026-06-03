# Session Summary — bd-53239e (agent stop endpoint transport-nonresponse wedge)

## Bead
**bd-53239e** (P2 bug; oracle complexity 4/5, risk 3/5, profile caco-web)
"ms-dev daemon agent stop endpoint wedged: transport nonresponse before semantic response
(blocks recreate of stuck caco-web)"

The ms-dev daemon's `POST /api/v1/agents/<id>/stop` failed at the transport layer before any
semantic response, while `/api/v1/node` and all read endpoints answered normally. This
blocked first-party recovery (`caco agent recreate` / `stop`) of a caco-web specialist stuck
in `starting`, since the stop step hit the wedged handler.

## Root cause (daemon-side)
The agent stop handler already wraps the stop call in a timeout
(`handle_agent_stop` → `tokio::time::timeout(stop_deadline, state.agents.stop())`, bd-4a6aad)
and returns 202 ACCEPTED with a background completer on timeout. **But that timeout was
ineffective**: `AgentManager::stop` is an `async fn` whose teardown body is a chain of
**synchronous blocking** subprocess calls run directly on the async worker thread:

- `verify_tmux_alive_on` / `kill_tmux_session_on` → `output_bounded` →
  `child.wait_timeout(...)` (a synchronous blocking wait, up to
  `CACO_AGENT_CONTROL_HELPER_TIMEOUT_SECS`=5s each),
- `checkpoint_dirty_work` (blocking git),
- `cleanup_checkout_processes` (process-tree walk + `std::thread::sleep`),
- microVM teardown.

These have **no `.await` yield point**, so the surrounding `tokio::time::timeout` future is
never polled while the worker thread is blocked. On a wedged tmux socket (or a never-settled
`starting` agent) with concurrent stuck stops, every Tokio worker thread blocks → the daemon
returns **transport nonresponse before any semantic response** on lifecycle endpoints while
read paths still answer. The ms-dev uptime (~1h48m) ≈ caco-web wedge duration matched this.

## Fix (`crates/caco-daemon/src/agent/lifecycle.rs`)
Extracted the synchronous blocking teardown span of `AgentManager::stop` into a module-level
free function `stop_teardown_blocking(snapshot, agent_dir, agent_id) -> Result<TeardownOutcome>`
(returning `was_alive` / `prior_state` / `forced_kills`, preserving both early-error exits),
and run it via `tokio::task::spawn_blocking(...).await`. This moves the blocking subprocess
work onto the dedicated blocking pool so:

- the async worker thread stays free (the runtime keeps serving HTTP),
- the handler-level `tokio::time::timeout` (bd-4a6aad) can actually preempt a wedged teardown
  and return 202 ACCEPTED instead of a transport nonresponse,
- a join failure surfaces as a clean `DaemonError` rather than a hang.

Behavior is otherwise unchanged: it still holds no agent-manager lock during teardown
(bd-3cd71b), preserves the kill/verify/retry sequence, the forced-kill accounting, and the
two early-error returns (tmux-alive kill failure; session-survived-kill).

## Scope discipline
This is a **code fix only**. The live ms-dev wedge's operator-gated workaround (ms-dev daemon
restart, which disrupts 8 healthy workers) is NOT performed here — that remains an
operator/controller decision. This change makes future stops on stuck-starting / wedged-tmux
agents recoverable without a daemon restart.

## Validation (queued on shared host)
- `cargo check -p caco-daemon --lib` → passed.
- `cargo test -p caco-daemon --lib stop_` → 25 passed (teardown path behavior preserved).
- `cargo clippy -p caco-daemon --lib -- -D warnings` → passed.

## SPEC
Preserves the agent-lifecycle transport contract (bounded lifecycle handlers, 202-ACCEPTED
async stop on slow teardown) and the local loopback responsiveness contract — a stale/wedged
runtime must not turn into daemon API transport failures.

## Diff
Landed squash SHA: see reintegration receipt.

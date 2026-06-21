# Session summary — caco agent nudge client timeout alignment (bd-2ba5ed fix a)

## Goal

Land the quick, contention-independent CLI-side win for the `/nudge` endpoint
failure (bd-2ba5ed): align the `caco agent nudge` client HTTP timeout to be >=
the daemon's nudge request bound, so the client waits for the daemon's bounded
structured backpressure response under store-lock contention instead of giving
up early and mis-classifying a slow-but-alive nudge as a transport failure.

## Bead(s)

- `bd-2ba5ed` — /api/v1/agents/<id>/nudge endpoint fails (transport nonresponse) under daemon load. This lands FIX (a) (client timeout alignment); the deeper lock-contention root (FIX (b)) remains tracked under `bd-057f2e`, so this does NOT close the bead.

## Before state

- Failing tests: none.
- Client `AGENT_NUDGE_HTTP_TIMEOUT_SECS = 8` (caco-cli/src/lib.rs:175), used as the total reqwest request timeout in `dispatch_agent_nudge`.
- Daemon nudge bound `DEFAULT_AGENT_NUDGE_REQUEST_TIMEOUT_SECS = 15` (caco-daemon), with the handler bounding the agent-manager lookup at ~14s before returning a structured backpressure response.
- So under store/agent-manager lock contention the daemon takes ~14-15s to respond, but the client gave up at 8s → `error sending request` / `endpoint_failed_before_semantic_response` (ctrl reproduced 5x across 1.2.1328/1.2.1329; blocked active agent-wake of idle devs).

## After state

- Failing tests: none.
- `AGENT_NUDGE_HTTP_TIMEOUT_SECS = 20` (>= the 15s daemon bound + 5s transit margin), with an explanatory comment. Still bounded (preserves bd-b8934e's controller-health-probe intent) but now waits for the daemon's structured response under contention.
- +1 regression-guard test (`agent_nudge_client_timeout_at_least_daemon_bound_bd_2ba5ed`) asserting the client timeout stays >= the daemon's 15s bound.
- Validation (queued): `cargo check -p caco-cli --tests` succeeded; `cargo test -p caco-cli --lib agent_nudge_client_timeout` = 1 passed.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-cli/src/lib.rs`.
- `AGENT_NUDGE_HTTP_TIMEOUT_SECS` 8 → 20 + comment; +1 regression test.
- Behavioural delta: under contention the nudge client now receives the daemon's structured backpressure response (a bounded, retryable semantic result) instead of a transport nonresponse at 8s. No change to the normal (uncontended, fast-return) nudge path.

## Operator-takeaway

This is FIX (a) of bd-2ba5ed — a contention-INDEPENDENT client-side win (worth
doing regardless, per ctrl). It does NOT eliminate the underlying slowness: the
nudge is slow under store/agent-manager LOCK CONTENTION, which is the deeper
common root tracked under bd-057f2e (likely behind the nudge timeout, bd-list
read timeouts, snapshot-refresh wedging, and slow control-plane reads). bd-2ba5ed
stays OPEN for FIX (b) (the contention) coordinated with bd-057f2e's owner. The
code-verify gate is what turned bd-2ba5ed from a wrong "extend EPIPE hardening to
nudge" premise into this correct, surgical client-timeout fix + the bd-057f2e
root link.

# Session summary 0032 — bd-b4f748 caco-web per-request logs

## Goal
Continue burndown post protocol-audit (operator directive 2026-04-24). caco-cli still on broken-on-main hold for bd0502bc stack-overflow, so pick non-caco-cli scope.

## Bead(s)
- **bd-b4f748 CLOSED** — caco-web per-request logs: axum middleware logs method, path, status, wall-clock latency with a monotonic per-process req=N correlation id. Default ON; opt-out via `CACO_WEB_REQUEST_LOG=0`/off/false/no. Applied after cors + compression so the logged status matches what the client saw. 5 pin tests (serial on env-var). 195/195 caco-web --lib.

## Diff summary
- `crates/caco-web/src/server.rs`:
  - +`REQUEST_ID: AtomicU64` (monotonic req-id counter, starts at 1)
  - +`fn request_log_enabled()` (env-var gate, default-on)
  - +`async fn per_request_log_middleware(...)` (axum middleware)
  - Router wiring: `.layer(axum::middleware::from_fn(per_request_log_middleware))` after cors+compression.
  - +`#[cfg(test)] mod tests` with 5 pin tests.
- `crates/caco-web/Cargo.toml`: +`serial_test.workspace = true` under dev-dependencies (already workspace-pinned for caco-tui).

## Operator-takeaway
- Live caco-web traffic now grep-able: `journalctl -u caco-web | grep '\[caco-web\] req='` shows full request stream with latency.
- Correlation between "request started" and "request finished" is a single line per request (no multi-line tracing spans needed) — matches the existing `[caco-web]` log-prefix convention and stays greppable from stdout.
- No new runtime dependency. `tower-http::trace` + `tracing` would be cleaner long-term but require a subscriber; bd-b4f748 was filed for "log-emission" not "structured-tracing migration". Leaving room for a follow-up bead if the operator wants tower-http::trace.
- Env-var gate pattern: unknown values leave logs ON (docker/systemd convention). Only explicit "off"-style values silence.

## Before state
- caco-web emitted only `[caco-web] Dashboard listening on http://...` at boot + bd-3af67d bind-retry messages. No per-request visibility.
- Debugging a stalled API call required daemon-side logs + browser-side devtools correlated manually.

## After state
- Every HTTP request passing through the web router produces a line like:
  `[caco-web] req=42 GET /api/v1/nodes -> 200 23ms`
- Default ON; env-var opt-out for noisy-log operators.
- Session tally: 23 closed/landed + 3 reopened-with-notes + 6 follow-ups + 6 regression pin tests added.
- Next summary index: 0033.

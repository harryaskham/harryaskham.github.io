# Session summary — retryable beads proxy failures

## Goal

Make the short ms-mac to Helsinki beads proxy 502 burst after authority recovery actionable instead of alarming: determine whether it represented durable board loss, and adjust daemon responses so future recovery-tail proxy failures carry retry/diagnostic metadata rather than opaque HTTP 502s.

## Bead(s)

- `bd-b79a53` — Investigate ms-mac cross-project beads sync 502 burst after Helsinki recovery

## Before state

- ms-mac log-monitor saw a bounded burst of beads/proxy HTTP 502s after Helsinki authority recovery, including cross-project `/beads/sync` requests.
- The same sweep showed no durable sentinel visibility failure: `caco bd status` recovered, active primary was Helsinki, sync was fresh/in-progress, and sentinel beads resolved.
- Existing proxy network failures returned opaque 502 `proxy_error` envelopes unless a declared maintenance window was active.

## After state

- Treated the incident as recovery-tail proxy/retryability noise rather than durable board corruption, because the authority and sentinel visibility recovered.
- Transient network failures reaching the active beads primary now return HTTP 503 with `Retry-After: 5`, code `beads_proxy_unavailable`, `retryable: true`, classification `transient_beads_proxy_unavailable`, `original_error`, `beads_proxy` diagnostics, and recovery guidance.
- Transient upstream 502/503/504 responses from the authoritative beads service now return HTTP 503 with `Retry-After: 5`, code `beads_authoritative_unavailable`, `retryable: true`, classification `transient_beads_authoritative_unavailable`, upstream status/body/target diagnostics, and the same proxy diagnostics.
- Auth failures remain specific (`401`/`403` classification) rather than being folded into retryable transient handling.

## Diff summary

- Commits: `a2e9c08d02`.
- Files touched: `crates/caco-daemon/src/beads.rs`.
- Tests: +2 daemon unit tests covering retryable 503 handling for direct proxy network errors and upstream 502s.
- Validation: `cargo fmt --all -- --check`; queued `tj-0f31c694` and `tj-ab410770` both passed `cargo test -p caco-daemon bd_b79a53 --lib`.
- Behavioural delta: future recovery-tail beads proxy instability should show retryable 503 maintenance-style metadata instead of cross-project opaque 502 bursts.

## Operator-takeaway

The post-recovery 502 burst was not evidence of another reduced-board incident; it was an observability gap in transient proxy failure handling. The daemon now labels those paths as retryable and includes enough routing details for log-monitor/controller sweeps to distinguish benign recovery-tail retries from actionable authority failures.

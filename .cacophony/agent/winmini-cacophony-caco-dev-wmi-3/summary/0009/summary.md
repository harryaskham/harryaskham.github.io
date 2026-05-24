# Session summary — bd-254f70 caco-web snapshot 502 sentinel

## Goal

Stop caco-web from logging raw HTTP 502 bursts for `/api/v1/ui/snapshot` when daemon/backpressure churn causes a snapshot proxy request to fail before any upstream response is available.

## Bead(s)

- `bd-254f70` — caco-web snapshot endpoint emitted fresh 502 burst on ms-mac

## Before state

- Log-monitor evidence showed repeated `GET /api/v1/ui/snapshot -> 502` lines from caco-web during ms-mac daemon backpressure windows.
- Existing generic proxy code already translated snapshot upstream 5xx and generic snapshot send failures into console-clean JSON sentinels, but the process-local snapshot cache fast path had its own pre-response `req.send()` error branch that could still return raw HTTP 502 for non-timeout, non-connect request errors.

## After state

- `refresh_snapshot_cache_from_daemon` now returns a `200 OK` JSON sentinel with `error:"daemon_proxy_upstream_error"`, `backend_unavailable:true`, and `upstream_status:502` for cached snapshot fast-path send failures that are neither timeout nor connect errors.
- The regression test now asserts that this cached snapshot fast path is covered by the snapshot request/read error sentinel contract.
- Focused queued validation passed in job `tj-07706b8b`: `CARGO_BUILD_JOBS=2 cargo test -p caco-web proxy_translates_snapshot_request_and_read_errors_to_sentinels_bd_66b513 -- --test-threads=1`.

## Diff summary

- Code/content commits: `f34ec6490` (`bd-254f70: return snapshot sentinel for cached proxy send errors`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-web/src/proxy.rs`, `crates/caco-web/src/tests.rs`
- Tests: +0 / -0 / flipped 0; one existing caco-web regression test strengthened
- Behavioural delta: browser/dashboard snapshot requests that fail in caco-web's cached refresh path now degrade like other snapshot failures instead of producing raw 502 responses and red browser/network log noise.

## Operator-takeaway

The likely recurrence path was the snapshot cache refresh branch, not the generic proxy branch. This fix closes that gap so ms-mac daemon backpressure should produce handled snapshot sentinels rather than caco-web 502 bursts for pre-response send failures.

# Session summary — TTS spoken-name transport diagnostics

## Goal

Make the recurring TTS `daemon_beads_all` spoken-name lookup failure actionable after it reappeared post-`bd-4dbb61`: when the daemon/service is otherwise healthy but the TTS daemon cannot fetch `/api/v1/beads/all?limit=5000`, the status/log diagnostic should identify the transport class and underlying source chain rather than a generic request failure.

## Bead(s)

- `bd-9552ca` — TTS daemon_beads_all failures recur after bd-4dbb61 close

## Before state

- Log-monitor observed `on-demand spoken-name refresh failed (retry in 900s): source=daemon_beads_all endpoint=http://127.0.0.1:11100/api/v1/beads/all?limit=5000 detail=spoken-name bead lookup failed: error sending request...` while `caco status` and `caco service status` were healthy.
- Existing HTTP failure diagnostics already carried source, endpoint, HTTP status, upstream status, and body snippets, but request/transport failures only exposed reqwest's top-level string.
- Acceptance required either successful recovery or a bounded degraded reason identifying the actual upstream error, with regression/diagnostic coverage.

## After state

- TTS spoken-name transport failures now append a reqwest class list (`connect`, `timeout`, `request`, `body`, `decode` as applicable) plus the full error source chain to the existing source/endpoint diagnostic.
- `README.md`, `SPEC.md`, and `AGENTS.md` document that spoken-name lookup health includes transport/source-chain detail.
- Focused validation passed: `tj-6b211afd` ran `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib tts_daemon_spoken_name_beads_lookup_reports_transport_source_chain_bd_9552ca -- --nocapture` successfully.
- Earlier validation `tj-a54c789b` failed because the new test was missing an import; the import was fixed before the passing run.

## Diff summary

- Commits: `9ae2784100`
- Files touched: `crates/caco-cli/src/lib.rs`, `README.md`, `SPEC.md`, `AGENTS.md`
- Tests: +1 focused async caco-cli regression test for spoken-name transport diagnostics.
- Behavioural delta: future TTS spoken-name lookup failures against `daemon_beads_all` remain bounded/backed off, but their error text now distinguishes local transport/request/source-chain failures from HTTP/upstream failures.

## Operator-takeaway

This does not make `/api/v1/beads/all` impossible to fail; it makes the failure self-diagnosing in `caco tts status` / logs so controllers can tell whether the recurrence is listener reachability, timeout, request/body/decode, or HTTP/upstream routing without waiting for another forensic bead.

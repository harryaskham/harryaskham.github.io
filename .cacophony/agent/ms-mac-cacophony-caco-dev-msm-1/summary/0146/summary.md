# Session summary — TTS spoken-name refresh avoids UI snapshot

## Goal

Fix `bd-e04320`, where the headless TTS daemon repeatedly refreshed bead spoken names through the heavyweight `/api/v1/ui/snapshot` endpoint and produced 60-second timeout warnings during daemon load. The goal was to keep spoken-name lookup best-effort and bounded so it cannot contend with normal speech/audio handling.

## Bead(s)

- `bd-e04320` — TTS spoken-name refresh repeatedly times out on daemon UI snapshot.

## Before state

- Failing tests: no pre-existing focused regression for this exact timeout path.
- Relevant metrics: ms-mac logs showed repeated TTS daemon spoken-name refresh failures against `/api/v1/ui/snapshot`, including HTTP 500/request timed out after 60s, plus one 85s speech request timeout during the same overloaded window.
- Context: `SPEC.md` §15 requires TTS spoken-name refresh to be best-effort with bounded backoff/diagnostics and not repeatedly hammer the full UI snapshot path during startup/backpressure.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: queued `caco-cli` focused test `tj-5b071499` passed for the updated lookup metadata/error handling; queued parser test `tj-dd83f3b9` passed; queued `cargo check -p caco-cli --lib` passed as `tj-472b27f7`; post-rebase focused validation passed again as `tj-5cf536f3`.
- Context: TTS spoken-name refresh now calls the narrower `/api/v1/beads/all?limit=5000` endpoint with a five-second request timeout and reports `daemon_beads_all` in snapshot-health metadata. Existing backoff/recovery behavior remains intact.

## Diff summary

- Commits: `6fd6b15366`, `0e89abf625`.
- Files touched: `crates/caco-cli/src/lib.rs`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-1/summary/pending/summary.md`.
- Tests: +0 net tests, but existing focused TTS spoken-name tests were updated to exercise the narrower endpoint.
- Behavioural delta: the TTS daemon no longer depends on the full UI snapshot path just to rewrite bead IDs in spoken messages, and it will fail quickly/degraded if the narrower bead lookup is unavailable.

## Operator-takeaway

Spoken-name refresh is now a small bounded lookup instead of a full-dashboard snapshot request, so TTS should stop amplifying daemon load or delaying speech during recovery/backpressure windows.

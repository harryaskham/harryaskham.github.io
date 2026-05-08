# Session summary — bd-b6f7ef agent nudge retry

## Goal

Fix a controller-observed reliability bug where `caco agent nudge` could report a stale daemon restart-window error even though the local daemon and supervisor status surfaces were healthy immediately before and after the nudge attempt.

## Bead(s)

- `bd-b6f7ef` — caco agent nudge reports restart stuck while status is healthy

## Before state

- Failing tests: none reproduced locally; the operational symptom came from controller health sweeps.
- Relevant metrics: `caco agent nudge --id ms-mac:cacophony:ms-mac-cacophony-caco-aks ...` had returned `daemon restart appears stuck ... listener still unreachable after the expected bind window` while `caco status` and `caco service status` were healthy.
- Context: `dispatch_agent_nudge` used a one-shot blocking `reqwest` send, so a brief daemon listener gap could immediately fall through to the sidecar lifecycle fallback and surface stale restart-window wording.

## After state

- Failing tests: none in the focused validation.
- Relevant metrics: queued validation `tj-113a9877` passed `RUST_MIN_STACK=33554432 cargo test -p caco-cli agent_nudge_uses_blocking_daemon_read_retries_bd_b6f7ef --lib -- --nocapture`.
- Context: cloneable blocking daemon requests now use a bounded retry helper before transport errors consult sidecar lifecycle fallback; `caco agent nudge` routes through that helper.

## Diff summary

- Commits: `584b6aa2ca`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added one source-level regression test pinning the retry helper and `dispatch_agent_nudge` call path.
- Behavioural delta: transient sub-second daemon read gaps in `caco agent nudge` are retried like other daemon read paths, reducing false restart-stuck reports during supervised restarts while preserving the existing sidecar fallback for persistent transport failure.

## Operator-takeaway

The controller nudge path now gets the same bounded read-retry posture as other CLI daemon requests before it declares a restart window stuck, so healthy post-restart nodes should no longer block recovery nudges on stale sidecar wording from a single listener-gap attempt.

# Session summary — bd-aedb1e restart-pending advisory false positives

## Goal

Stop the restart-pending peer-health advisory from implying replication starvation or recommending force-restart remediation when the only signal is a stale config-hash/restart-pending probe.

## Bead(s)

- `bd-aedb1e` — bd-67876b restart-pending advisory false-positives when hash-endpoint stalls but replication is healthy

## Before state

- A peer stuck in `restart_pending == true` beyond the stale threshold became `actionable` even when the only evidence was hash mismatch.
- The note said “replication may be starved” and recommended force-restarting the peer if manual count checks diverged.
- Operators observed healthy replication moving in lockstep while this advisory persisted for hours, making the wording/actionability misleading.

## After state

- Hash-only restart-pending mismatch is explicitly advisory and no longer flips the peer into the actionable outage bucket.
- The note now states that stale restart-pending is a hash-only advisory, not replication-divergence evidence.
- The stale-threshold note asks operators to verify bead counts before remediation and avoids “replication may be starved” / “force-restart” wording based solely on stale hash data.
- Updated the daemon regression test to enforce the new advisory semantics.

## Diff summary

- Commit: `53cf5956e` after replay onto the remote agent branch.
- Files touched: `crates/caco-daemon/src/lib.rs`.
- Tests: `cargo test -p caco-daemon peer_health_annotation_keeps_stale_restart_pending_advisory_bd_aedb1e --lib`; `cargo check -p caco-daemon --tests`; `cargo fmt --all -- --check`; `git diff --check`.
- Behavioural delta: peer-health summaries distinguish stale hash lifecycle state from proven replication divergence.

## Operator-takeaway

A stale restart-pending hash probe will no longer push operators toward destructive peer restarts unless another sensor actually proves divergence.

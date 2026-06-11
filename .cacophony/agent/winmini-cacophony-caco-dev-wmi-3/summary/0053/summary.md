# bd-e207bb — spawn_blocking the lighter same-class summaries local-checkout enumerator + resume_inner self-heal git

## Bead
bd-e207bb (daemon-resilience/disk-io/spawn-blocking/follow-up, P3; filer aurora-3, per msm-2 review). Optional same-class follow-up to bd-3d5cdf (landed 7dda327796, which decoupled the state-branch git in the summaries read handlers). Two lighter bare-async-sync-git-on-the-worker instances remained; offload both to the blocking pool so a disk-I/O-contended checkout does not park a tokio worker.

## Fixes
### 1. handle_summaries_list local/default-checkout enumerator (crates/caco-daemon/src/lib.rs)
The local-checkout enumeration (`enumerate_summary_keys_from_checkout` + `enumerate_summary_candidates_from_checkout_page`) still ran bare-async (the state-branch path was already wrapped by bd-3d5cdf). Wrapped both in one `tokio::task::spawn_blocking` returning `(BTreeSet<SummaryKey>, SummaryCandidatePage)` (both owned/Send), mirroring the bd-3d5cdf state-branch block but WITHOUT an inner timeout (no network fetch on the local checkout, so the worker-decouple alone is the fix and a slow local read should not shed a 503; the outer request timeout still bounds it). EnumerateFilter borrows, so an owned filter is reconstructed inside the closure. JoinError (panic) → summaries_backpressured_response (defensive, consistent with the state-branch error path).

### 2. resume_inner self-heal/cross-node checkout rebuild (crates/caco-daemon/src/agent/lifecycle.rs)
The missing-checkout self-heal recovery path (both the cross-node and same-node arms) did `run_git(fetch into canonical) + create_shared_clone + (on success) run_git(fetch) + run_git(reset --hard)` bare-async. Verified the registry lock (`self.inner`) is NOT held during this git span (it is only taken in the short scoped update blocks AFTER the clone), so the span is safely offloadable. Wrapped each span in an inline `spawn_blocking` closure returning `Result<(), DaemonError>` (create_shared_clone already returns DaemonError, so `?` propagates and the call-site error messages are preserved); JoinError → a clear panic DaemonError. The scoped lock-update / blocker-persist / return-Err control flow after the clone is unchanged.

## Validation (daemon test queue, --cwd at checkout)
- `cargo clippy -p caco-daemon --lib` (tj-e604dfaa): PASSED (exit 0) — both spawn_blocking decouples compile clean (Send bounds + control flow) with no clippy warnings in lifecycle.rs / lib.rs.
- `cargo test -p caco-daemon --lib summary` (tj-9256c81b): PASSED — 124 summary tests pass, 0 failed (the local-checkout enumerate path still produces correct keys/candidates through the spawn_blocking).
- rustfmt-clean on changed regions (the whole-file "would reformat" is pre-existing drift elsewhere, left untouched); `git diff --check` clean.
- The change is behavior-preserving (same git/fs ops, same control flow, just off the async worker), matching the bead's accepted bar (compile + existing summaries/resume tests). The under-contention worker-non-parking benefit is the goal; no behavior change to verify live.

## Scope
Rebased onto true upstream first to pick up bd-3d5cdf (local mirror was stale) so the state-branch enumerate was already wrapped and my change only touched the still-bare local-checkout + resume self-heal spans (no double-wrap).

## Diff
See the reintegration receipt for the landed squash SHA.

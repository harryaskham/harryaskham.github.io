# Session summary — source-side idle advisory stderr mirror guard

## Goal

Stop the caco-aks persistent-idle advisory WARNs from reaching `daemon-crash.log` even if a future or stale call path uses the normal EventLogger stderr mirror rather than the already-safe `log_without_stderr_mirror` path.

## Bead(s)

- `bd-d4bcd3` — caco-aks idle advisory still writes to daemon-crash.log after bd-5bceb1 close

## Before state

- Failing tests: no trustworthy product failure; an initial queued daemon logging validation was interrupted by a daemon restart and reported retryable infrastructure error `tj-88583f0b`.
- Relevant metrics: log-monitor evidence showed 18 timestamped WARN idle-advisory lines in the 2026-05-08T23:41:31Z → 2026-05-08T23:56:31Z crash-log sweep, including caco-aks examples at 23:55:21.260Z and 23:56:07.613Z.
- Context: previous beads hardened the sidecar stderr router and file write path. This recurrence suggested the safer invariant belongs at the source-side EventLogger mirror gate too.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused daemon logging validation and sidecar router validation both passed after retrying past the daemon restart.
- Context: EventLogger now has a source-side `should_mirror_stderr_line` guard that suppresses warning-only persistent-agent idle advisory messages from stderr while preserving existing stderr behavior for ordinary non-advisory warnings and real crash-like output.

## Diff summary

- Commits: local agent commit before final summary amend: 8420e5776d; final landed squash appears in the reintegration receipt.
- Files touched: `crates/caco-daemon/src/logging.rs`
- Tests: added `persistent_idle_advisory_is_never_stderr_mirrored_bd_d4bcd3`.
- Behavioural delta: persistent-agent idle advisories matching the observed caco-aks shape no longer mirror to stderr/`daemon-crash.log` through EventLogger, even if a caller uses the general logging path; debug logs remain non-mirrored and unrelated warnings still mirror as before.
- Validation: `git diff --check`; `tj-ea30ebba` passed `cargo test -p caco-daemon persistent_idle_advisory_is_never_stderr_mirrored_bd_d4bcd3 --lib -- --nocapture`; `tj-05301393` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`; post-rebase `tj-082ec0c1` and `tj-033a64ef` passed the same focused daemon and sidecar subsets. Initial `tj-88583f0b` was a retryable daemon-restart infrastructure error and not a test failure.

## Operator-takeaway

This moves the idle-advisory crash-log defense closer to the source: routine persistent-agent idle warnings should be kept out of daemon stderr before the sidecar router ever has to classify them.

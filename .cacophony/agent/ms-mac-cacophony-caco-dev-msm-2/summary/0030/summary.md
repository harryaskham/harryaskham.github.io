# Session summary — ms-mac shared-clone retry hardening

## Goal

Keep ms-mac persistent agents healthy by reducing false startup/reconcile failures from transient shared-clone errors during busy restart windows. The session also included urgent TTS routing coordination; that was handled at runtime and left to the dedicated `bd-d87c61` owner, while this committed chunk focuses on `bd-d0e6f0`.

## Bead(s)

- `bd-d0e6f0` — Persistent agent caco-dev-msm-2 fails at startup and periodic reconcile (git clone --sha launch error)

## Before state

- Failing tests: none known for this patch. Concurrent broken-on-main failures were owned by other agents.
- Relevant metrics: current msm-2 was recovered/running with no `last_error`, but daemon logs showed repeated historical `Persistent agent caco-dev-msm-2 failed during startup/periodic reconcile` events clustered around restart/load windows.
- Context: agent checkout creation used a single `git clone --shared` attempt. On macOS under heavy persistent-agent recreation, transient git/process/lock failures such as `Resource temporarily unavailable`, `config.lock`, `index.lock`, or fork/thread exhaustion could surface as persistent launch failures until a later reconcile.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: `cargo test -p caco-daemon shared_clone_retry_classifies_transient_git_failures --lib` passed; `cargo test -p caco-daemon shared_clone_uses_shared_objects --lib` passed; `cargo fmt --all -- --check` passed; `cargo check -p caco-daemon --tests` passed.
- Context: shared clone now retries transient clone/lock/resource errors up to three attempts with short bounded backoff and removes partial destination directories between attempts. Non-transient errors still fail immediately.

## Diff summary

- Commits: `c49eba231`
- Files touched: `crates/caco-daemon/src/agent/spawn.rs`, `crates/caco-daemon/src/agent/tests.rs`
- Tests: +1 unit test covering transient-vs-permanent shared clone error classification.
- Behavioural delta: persistent agent startup is more tolerant of short-lived macOS git/process pressure without hiding real repository or configuration errors.

## Operator-takeaway

The immediate msm-2 instance is healthy, but this patch hardens the next restart/reconcile wave: transient `git clone --shared` failures should retry in-place instead of producing avoidable persistent-agent failure churn.

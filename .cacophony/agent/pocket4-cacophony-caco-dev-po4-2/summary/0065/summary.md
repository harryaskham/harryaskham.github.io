# Session summary — bd-a51938 worker-origin true-upstream profile env fix

## Goal

Continue the worker-origin true-upstream rollout by validating the stage-1 `worker-origin-upstream-test` profile path and fixing any daemon-code gap that prevented a single test worker checkout from using the true upstream remote for ordinary `git fetch origin main` while preserving the canonical object borrow.

## Bead(s)

- `bd-a51938` — Daemon: worker `origin` must not serve a stale local canonical mirror — fetch true upstream or shorten mirror cadence.

## Before state

- The Option B core implementation and test profile already existed on main, but live validation attempts using `worker-origin-upstream-test` produced worker checkouts whose `origin` still pointed at `/home/harry/.cacophony/daemon/checkouts/cacophony`.
- Live validation also exposed stale lifecycle create operations stuck in `running`; I filed draft `bd-16e011` for that separate operation-timeout/reaper gap rather than expanding this bead.
- Current branch had no source changes for this bead before this slice.

## After state

- `AgentManager::create` now resolves `CACO_WORKER_ORIGIN_TRUE_UPSTREAM` from both request/project env and resolved profile env at checkout-clone time, before the later `env.sh` runtime merge.
- `worker-origin-upstream-test` profile env can now enable the per-spawn origin repoint without needing a daemon-global default flip or an explicit request-body env override.
- Existing request-env opt-in and default-off behavior are preserved.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-daemon/src/agent/spawn.rs`
  - `crates/caco-daemon/src/agent/lifecycle.rs`
  - `.cacophony/agent/pocket4-cacophony-caco-dev-po4-2/summary/pending/summary.md`
- Tests: added one focused unit test covering profile-env clone-time flag resolution; existing per-spawn env test still passes.
- Behavioural delta: profile-owned `env: CACO_WORKER_ORIGIN_TRUE_UPSTREAM=1` now affects the actual checkout topology decision, not only the runtime shell environment after checkout creation.

## Operator-takeaway

The stage-1 validation profile was correctly setting the feature flag, but the daemon made the clone/repoint decision before merging profile env; this slice threads that profile env into the clone-time decision so a single test spawn can validate Option B without globally changing fleet behavior. The remaining stale-running lifecycle-operation observation is tracked separately as draft `bd-16e011`.

## Validation

- `tj-efb2ce5d` passed: `cargo test -p caco-daemon --lib worker_origin_true_upstream_from_env_layers_reads_profile_env_bd_a51938 -- --test-threads=1`.
- `tj-8c5b2e53` passed: `cargo test -p caco-daemon --lib worker_origin_true_upstream_from_env -- --test-threads=1`.
- `tj-bbc3aed7` passed: `cargo check -p caco-daemon --lib`.
- `git diff --check` passed.
- Note: these commands still surface a pre-existing warning in `crates/caco-daemon/src/agent/lifecycle.rs` about `was_non_terminal` being assigned but not read; this slice did not introduce that warning.

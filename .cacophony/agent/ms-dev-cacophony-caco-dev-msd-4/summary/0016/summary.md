# Summary 0016 — bd-274c2d permanent test-health cycle

## Bead
bd-274c2d (P1, permanent, task) — continuous test suite health.
Cycle observation + small drive-by clippy fix.

## Probe results

`cargo test-small` (workspace) — 4244 tests pass:
- caco-beads:    211 ok
- caco-bead:     109 ok
- caco-config:   739 ok
- caco-profile:  295 ok
- (small):        18 ok
- caco-daemon:  2818 ok
- caco-cli:       56 ok

This is a marked recovery from earlier in the night when caco-tui
was broken-on-main from bd-7ef076's tmux_history_* fields landing
without test-fixture propagation. msd-1 fix-forwarded that with
bd-1c0bdd / bd-bce6ea (commit 667cab25), restoring test-small to
fully green.

`cargo clippy --workspace --no-deps` — 2 warnings remain:
- `caco-cli`: "this function has too many arguments (15/7)" —
  design-level, not a permanent-cycle drive-by. Belongs in a
  scoped refactor bead.
- (no other warnings)

## Drive-by fix

`crates/caco-beads/src/sync.rs`:

Two clippy `useless_format` warnings introduced by mab6fpzek's
bd-180c6d defensive ECHILD recovery (commit 65d92985). Replaced
`format!("...static literal...")` with `.to_string()` per clippy's
own suggestion. No behaviour change; both call sites still
populate `stderr: Vec<u8>` with the same bytes via
`.to_string().into_bytes()`.

This brings caco-beads to zero clippy warnings.

## Verification

- `cargo test-small` — 4244/4244 green (per above).
- `cargo test -p caco-beads --lib` — 211/211 green
  (drive-by didn't break sync.rs tests).
- `cargo clippy --workspace --no-deps` — only the pre-existing
  `caco-cli` 15-arg warning remains. caco-beads now clean.

## Pre-existing flakes / breakages still observed

These remain on the standing watch list from prior cycles:

- `caco-daemon::lib` test-binary intermittently SIGABRTs with
  stack overflow on
  `tests::discover_available_profile_names_includes_checked_in_canonical_profiles_without_checkouts`
  when run via `cargo test -p caco-daemon --lib` (full pre-link
  binary, not test-small subset). Not yet root-caused; bd-?
  candidate. Doesn't affect test-small.
- `caco-daemon::agent::tests::cleanup_checkout_processes_kills_orphans_and_reports_count`
  intermittently FAILS — orphan-cleanup race; pre-existing.
- caco-cli `agent_logs_*` (bd-fabf46), `bd_send_request_*` /
  `bd_daemon_result_routes_transport_error_envelope` SIGABRT
  (msm-1), `agent_attach_help_shows_id_and_raw_args` stack
  overflow, `caco-cli` ~83 env-isolation cluster (bd-8e16e0)
  — all pre-existing, tracked in their own beads.
- caco-sidecar `serve_eaddrinuse_reports_clear_error` (bd-30a15c).
- caco-daemon `persistent_recreate_*` (msm-2).

None are regressions from this cycle.

## Operational impact

- Workspace test signal is back to clean. Future cycles should
  prefer `cargo test-small` (the merge-queue mixin contract) and
  treat the listed flakes as known-known until their owners
  address them.
- caco-beads is now clippy-clean — small but meaningful for the
  growing crate (sync.rs is hot-path).

## Next

Reintegrate direct (summary-only artefact). bd-274c2d remains
permanent; idle until next cycle.

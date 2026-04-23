# Session summary — bd-98acfe: persist test_queue + build_queue across daemon restarts

## Goal

Direct follow-on to bd-7b19c1 (CLI-side stop-the-bleeding for
workers wedging on cargo test/build after daemon restart). bd-7b19c1
made the CLI escape after 3 consecutive HTTP 404s. This bead fixes
the underlying root cause: TestQueueManager and BuildQueueManager
were entirely in-memory; daemon restart wiped them and the orphaned
child cargo processes had no recovery path.

## Bead(s)

- **bd-98acfe** (P2 feature, owned).
- bd-7b19c1 (sibling — CLI half landed in earlier session).
- Drive-by: caco-tui-reported compile break in
  `crates/caco-sidecar/src/lifecycle.rs` (missing
  `peer_consult_timeout_ms` field on a `TopLevelBeadsConfig` struct
  literal). Fixed inline because I was already touching the
  workspace.

## Before state

- `TestQueueManager` / `BuildQueueManager` held all state in
  `Arc<Mutex<...Inner>>` with three `HashMap`s; zero on-disk
  persistence.
- Child cargo processes spawned with `kill_on_drop(true)` —
  reaped only on clean shutdown; SIGKILL/crash leaves orphans.
- Daemon restart → empty maps → every previously-known job ID
  returns HTTP 404 → CLI either silently loops (pre-bd-7b19c1)
  or fails fast with the "job lost" error (post-bd-7b19c1).
- `crates/caco-sidecar/src/lifecycle.rs:3491` had a
  `TopLevelBeadsConfig {...}` struct literal missing the new
  `peer_consult_timeout_ms` field, breaking workspace compile.

## After state

- New `PersistedQueueState { schema, jobs, counter }` /
  `PersistedBuildQueueState { ... }` written atomically
  (write-temp + rename) to `<artifacts_dir>/queue.json` and
  `<artifacts_dir>/build_queue.json` after every state transition
  (enqueue, cancel, drain Queued→Running, terminal).
- `TestQueueManager::new` / `BuildQueueManager::new` call
  `load_or_init`, which:
  * reads the snapshot, validates schema (current = 1),
  * marks every previously-`Running` job as `Error` with a clear
    `error_message` carrying the bead-id breadcrumb,
  * re-enqueues every previously-`Queued` job into the per-project
    pending FIFO so `drain_pending` picks it up on the next tick.
- Persist failures are logged but never propagated — the in-memory
  state stays authoritative while the daemon is alive.
- Corrupt JSON / schema mismatch on the on-disk snapshot are
  logged and ignored; the queue starts empty rather than
  wedging the daemon on startup.
- 5 new tests covering: queued-survives-as-queued,
  running-recovers-as-error, corrupt-ignored, schema-mismatch-
  ignored (test queue), running-recovers-as-error and
  corrupt-ignored (build queue).
- 33/33 test_queue tests + 27/27 build_queue tests pass; clippy
  clean on touched code.
- Workspace compile unblocked (TopLevelBeadsConfig field added).

## Diff summary

- Commit `e710eb7b`: bd-98acfe: persist test_queue + build_queue
  across daemon restarts.
- Files touched:
  - `crates/caco-daemon/src/test_queue.rs` (~+250: persist/load
    helpers, hook calls, 4 new tests).
  - `crates/caco-daemon/src/build_queue.rs` (~+200: mirrored
    persist/load, hook calls, 2 new tests).
  - `crates/caco-sidecar/src/lifecycle.rs` (+1: missing field).
- Tests: +6 / 0 flipped / 0 ignored. Pre-existing
  `discover_available_profile_names_includes_checked_in_canonical_profiles_without_checkouts`
  stack-overflows on the full lib test run; reproduced on
  pristine main and confirmed unrelated to this change.
- Behavioural delta:
  * Workers polling for a previously-running job after restart see
    `state=error, error_message=daemon restarted...` instead of
    HTTP 404 / silent timeout.
  * Workers polling for a previously-queued job see it still
    pending and it drains naturally on the next tick.
  * Together with bd-7b19c1's CLI-side `NOT_FOUND_THRESHOLD`
    fallback, the wedge mode is closed from both sides.

## Operator-takeaway

The bd-7b19c1 / bd-98acfe pair completes the daemon-restart-
robustness fix for queued cargo work. bd-7b19c1 stopped the
bleed (CLI escapes silent 404 loop); bd-98acfe removes the wound
(daemon now persists queue state and surfaces recovery transitions
explicitly to polling clients).

Future enhancement candidates intentionally NOT in this slice
(deliberately scoped to land cleanly):
- per-job PID file for orphan-cargo cleanup on startup
  (kill -0 detection, optional re-attach via stdout.log tail);
- debounced persistence for queues with high transition rates;
- structured `daemon_restart_recovered_at` / `recovery_reason`
  fields on the JSON envelope so the TUI can render a recovered-
  job indicator instead of just a generic Error.

Each of those wants its own bead.

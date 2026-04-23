# Session summary — bd-7b19c1: workers stuck on cargo test/build after daemon restart

## Goal

Root-cause + ship the smallest patch that breaks the reported wedge:
agent runtimes silently looping forever on `caco test`/`caco build`
after the daemon restarts mid-job. The bead labelled this an
"interceptor" issue; the investigation shows there is no interceptor
in the path — it's a missing-persistence + silent-404 bug.

## Bead(s)

- **bd-7b19c1** (P2 bug, owned).
- bd-d5e2bc — closed as duplicate of bd-7b19c1 (same symptom, same
  misleading "interceptor" framing).
- bd-98acfe — filed as the deeper daemon-side persistence follow-up
  (TestQueueInner / BuildQueueInner save+load, mark previously-
  running jobs as state=error reason=daemon_restarted on startup).

## Before state

- `TestQueueManager` and `BuildQueueManager` hold all state in a
  single `Arc<Mutex<TestQueueInner>>` with three `HashMap`s and zero
  on-disk persistence. Daemon restart wipes them entirely.
- Child cargo processes spawned with `kill_on_drop(true)` — only
  reaped on clean shutdown; SIGKILL/crash leaves them as orphans.
- `handle_test_show` returns HTTP 404 for unknown job IDs (correct).
- The CLI poll loop in `dispatch_test_run_wait` and the parallel
  `dispatch_build_run_wait` silently `continue`d on any non-2xx
  response, including 404. Workers polled the entire `wait_timeout`
  (default 600s) and returned a "still running" message; agent
  runtime treated it as soft-transient and retried → infinite loop.

## After state

- New: `docs/investigations/bd-7b19c1-test-queue-restart.md` with
  TL;DR, manual reproduction (launchctl/systemctl restart between
  enqueue and poll), code references, root cause, and a separate
  "no interceptors are involved; the framing is the bug" note.
- `crates/caco-cli/src/lib.rs`: both `dispatch_test_run_wait` and
  `dispatch_build_run_wait` now count consecutive HTTP 404 responses
  and, after `NOT_FOUND_THRESHOLD = 3` (~9s at the existing 3s
  `POLL_INTERVAL`), return a structured `CliError` calling out the
  likely cause and the recovery action ("re-submit the job"). The
  counter resets on any non-404 response, so a single transient
  blip during a daemon bounce does not nuke the wait.
- Tests: extended `test_run_wait_blocks_until_terminal` structural
  assertions to pin (a) the `NOT_FOUND_THRESHOLD` constant, (b) the
  explicit `StatusCode::NOT_FOUND` inspection (no more silent
  continue), (c) both the `bd-7b19c1: test job` and `bd-7b19c1:
  build job` error strings are in the binary.
- bd-d5e2bc closed via `caco bd update --duplicate-of bd-7b19c1`.
- bd-98acfe filed as the deeper follow-up (queue persistence +
  startup-time mark-as-error sweep + per-job PID file for orphan
  detection).

## Diff summary

- Commit `0ae06d98`: bd-7b19c1: surface daemon-restart-induced job loss
  instead of silent poll forever.
- Files touched:
  - `crates/caco-cli/src/lib.rs` — both poll loops + the structural
    test (~+90).
  - `docs/investigations/bd-7b19c1-test-queue-restart.md` — new (+86).
- Tests: 0 new tests, 4 new assertions inside an existing test
  (`test_run_wait_blocks_until_terminal`).
- Behavioural delta: workers no longer loop until `wait_timeout` after
  daemon restart — they fail fast (~9s) with an actionable error.
- 1092 caco-cli lib tests pass; clippy clean.

## Operator-takeaway

Two-pronged: stop-the-bleeding shipped this slice, deeper structural
fix filed (bd-98acfe) so the next agent knows where to pick up. The
investigation explicitly debunks the "interceptor" framing in both
bd-7b19c1 and bd-d5e2bc (now dup-closed): there is no interceptor
between caco-cli and the daemon's queue HTTP endpoints, the request
path is direct HTTP, and the wedge is purely the silent-404 in the
poll loop on top of an in-memory-only queue. Future bug reports of
this shape should be triaged toward "queue persistence" or "CLI
poll error-handling", not "interceptors".

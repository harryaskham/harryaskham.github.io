# Session summary — caco-web bind retry with backoff (bd-3af67d)

## Goal

Eliminate the ~2-3 minute caco-web outage during daemon-restart
events when the previous caco-web PID still holds port 11180.

## Bead(s)

- `bd-3af67d` — caco-web port 11180 collision after daemon restart (P2 bug)
- `bd-6b21ff` — closed as duplicate of bd-3af67d.

## Before state

- `serve()` called `TcpListener::bind(addr).await?` once.
- Any AddrInUse error → immediate exit; tts-watchdog
  (bd-5f8223) eventually reaped the stale process ~2.5min later.
- User-visible dashboard outage during restart.

## After state

- New `bind_with_retry(addr)` helper: 6 attempts with exponential
  backoff (1s, 2s, 4s, 8s, 15s, final = ~30s total).
- Only retries on AddrInUse; other errors (PermissionDenied etc.)
  fail immediately.
- Logs each retry attempt with bd-3af67d tag.
- 1 unit test: bind_with_retry_succeeds_after_port_freed —
  spawns a task that drops the holder after 1.5s, asserts
  bind_with_retry succeeds.

## Diff summary

- Files touched (+50 / −1):
  - `crates/caco-web/src/server.rs`: bind_with_retry helper +
    serve() change.
  - `crates/caco-web/src/tests.rs`: 1 test.

## Verification

- `cargo build -p caco-web`: OK.
- `cargo test -p caco-web --lib bind_with_retry`: 1 pass.
- `cargo clippy -p caco-web --lib --tests -- -D warnings`: clean.

## Operator-takeaway

Closes the most common cause of the dashboard-down-during-restart
window. Pairs with bd-5f8223 (watchdog reap path); this bead is
the *avoidance* angle. Worst case (port held > 30s) still falls
back to watchdog reap.

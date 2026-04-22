# Session summary — bd-7a33c5: structured logging on /beads/sync 500s

## Goal

POST /api/v1/projects/{p}/beads/sync was returning HTTP 500 at ~12/h sustained on ms-mac without any details in daemon.log beyond the bare HTTP error line. Add structured logging so the next observer can identify the actual phase + error.

## Bead(s)

- `bd-7a33c5` — POST /beads/sync -> 500 RESURGED — steady ~12/h cadence post-close of bd-ea6668 (acceptance criterion 2: structured error message added so root cause becomes visible).

## Before state

- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `handle_beads_sync` in `crates/caco-daemon/src/beads.rs` had two paths returning `internal_error(...)` (and one returning `bead_error_response(...)`) without any `eprintln!` tag, so daemon.log only saw the bare 500 with no project / phase / underlying error. Two pre-existing `eprintln!` lines for pull / push errors (downgraded to non-fatal) showed the pattern but the actual 500 paths didn't.

## After state

- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `cargo test-small`: 56 pass.
- All three 500-emitting branches in `handle_beads_sync` now `eprintln!` with the format:
  `[bd-7a33c5] beads-sync 500 project=<p> phase=<phase> request_id=<id> error=<e>`
  where `<phase>` is one of `cached_beads_store`, `reconcile`, `task_join`. The pre-existing pull/push eprintln lines (non-500 paths) are kept as-is.

## Implementation

- `crates/caco-daemon/src/beads.rs` — three new `eprintln!` lines in `handle_beads_sync`:
  - `cached_beads_store` failure (lookup of the per-project BeadsStore).
  - `reconcile` failure inside the spawn_blocking task (the JSONL ↔ DB round-trip, plus pull/push wrappers — the actual 500 source per Phase 1 of the handler).
  - `task_join` panic (tokio task aborted/panicked).
- Each line includes the bead-id tag, project name, phase label, the request-id (from the `x-caco-request-id` header), and the error display.

## Diff summary

- `crates/caco-daemon/src/beads.rs` — 3 `eprintln!` insertions inside `handle_beads_sync`.
- Commit: `<TBD>`.

## Operator-takeaway

This satisfies acceptance criterion 2 of bd-7a33c5 (structured error message in daemon.log so root cause is visible). Operator should grep `[bd-7a33c5] beads-sync 500` in daemon.log over the next sweep window — the `phase=` value will say whether it's a store-resolution miss (config / project-not-configured), a `reconcile` data-shape problem (the SnapshotBead round-trip just fixed in bd-10e37c was one such), or a panic in the spawn_blocking worker. Once root cause is visible, criteria 1 (rate drop) or 3 (trigger-fix) can follow as a focused bead. The handler doesn't change behaviour — same status codes, same response bodies — so no client-visible regression risk.

## Coordination notes

- Pairs naturally with the bd-10e37c reconcile fix from this session: that bug would have shown up in this log as `phase=reconcile error=db: snap-bd-... was already indexed` or similar. Now any future occurrence is observable instead of opaque.

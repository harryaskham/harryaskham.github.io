# Session summary — daemon file-cache content endpoint (bd-95eddd)

## Goal

The daemon file-cache API (`/api/v1/file-cache`) served upload + list metadata
only — it had no route to download blob CONTENT. caco-web serves blobs via its
own `/api/v1/web/files/<project>/<id>/content` proxy reading local storage
directly, which mobile clients (iOS/watch/Android) cannot use. Add a canonical
daemon content-download endpoint so every first-party client has a blob path at
parity with the web dashboard.

## Bead(s)

- `bd-95eddd` — Daemon: file-cache content endpoint
  (`GET /api/v1/file-cache/<id>/content`) for mobile blob download (P1 feature;
  labels api/companion/daemon/file-cache; lane Rust/daemon). Implemented +
  landed. Unblocks `bd-d812ad` (iOS Files download/save) and Android/watch
  parity.

## Before state

- `crates/caco-daemon/src/file_cache.rs` had only `handle_upload_file_cache`
  (`POST /api/v1/file-cache`), which shells out to the canonical `caco file add`
  writer.
- No daemon route exposed blob bytes; `/api/v1/file-cache` was already in the
  daemon auth-scope allowlist (bearer token + cluster mTLS).
- Failing tests: none in scope.

## After state

- New `GET /api/v1/file-cache/{file_id}/content` route registered in
  `crates/caco-daemon/src/lib.rs` next to the existing POST.
- New `handle_file_cache_content` handler in `file_cache.rs`: shells out to the
  canonical `caco file get --file-id <id> [--project <project>] --json` reader
  (the same subprocess pattern as the upload handler, avoiding duplication of
  the 40-field file-cache record schema in the daemon), with a bounded
  `FILE_CACHE_GET_TIMEOUT` (60s). It serves the decoded bytes with
  `Content-Type` from the record mime (fallback `application/octet-stream`),
  `Content-Disposition: attachment; filename="…"` from a header-safe sanitized
  record name, and `Cache-Control: no-store`. An optional `?project=<project>`
  query scopes the lookup; when omitted the reader resolves a globally-unique id.
- Error mapping: empty id → 400; reader non-zero exit (unknown id / blob not
  locally available) → bounded single-line 404; subprocess spawn error → 500;
  timeout → 504; malformed reader JSON → 500. stderr/payload are never echoed in
  full.
- Pure, unit-testable helpers: `parse_file_get_json` (tolerates both the wrapped
  `{ data: { file, content_base64 } }` and unwrapped shapes; mime/name fallbacks)
  and `safe_download_filename` (strips path components, quotes, backslashes, and
  control chars to prevent `Content-Disposition` header injection).
- Tests: +7 bd-95eddd unit tests (wrapped/unwrapped parse, id-fallback when name
  blank, missing-content rejection, missing-file-record rejection,
  filename-sanitization). Pre-existing file_cache upload tests unchanged.

## Diff summary

- Code commit: see the reintegration receipt for the final landed squash SHA.
- Files touched:
  - `crates/caco-daemon/src/file_cache.rs`: `FileCacheContentQuery`,
    `FileCacheContent`, `safe_download_filename`, `parse_file_get_json`,
    `handle_file_cache_content`, `FILE_CACHE_GET_TIMEOUT`, +7 tests.
  - `crates/caco-daemon/src/lib.rs`: register the content route.
- Tests: +7 / -0 / flipped 0.
- Behavioural delta: first-party clients can now download a project file-cache
  blob's bytes over the authenticated daemon API, not just via the caco-web
  local-storage proxy.

## Validation

- `cargo test -p caco-daemon --lib file_cache` (queued, PASSED — 15/15, incl. 7
  new bd-95eddd tests).
- `cargo clippy -p caco-daemon --lib` (queued, PASSED; new code warning-clean).
- `cargo check` is implied by the test/clippy compile (both built the lib).
- `scripts/rustfmt-changed.sh --check crates/caco-daemon/src/file_cache.rs` →
  clean; lib.rs carries pre-existing whole-file rustfmt drift left untouched (my
  route addition is rustfmt-clean). `git diff --check` clean.

## Follow-ups

- Client kit work (out of scope for this daemon bead): `DaemonClient
  .downloadFileCache(id) -> Data` and the iOS `FileDetailView`
  download/ShareLink/QuickLook action (bd-d812ad), plus Android/watch parity.

## Operator-takeaway

This is a daemon-only, additive read endpoint. It reuses the canonical
`caco file get` reader (so it stays in lockstep with the CLI/state-branch record
schema) and is bounded by a timeout. Blobs that are not locally available on the
serving node return 404, the same practical limit as the caco-web proxy.

# Session summary — summaries background loading

## Goal

Implement `bd-2dd008`: after the initial summaries metadata/first-detail batch renders, the web summaries viewer should fetch remaining pages asynchronously and progressively append rows without blocking the initial page.

## Bead(s)

- `bd-2dd008` — `Lazily load remaining summaries in background`

## Changes

- Updated `crates/caco-web/static/summaries.js`:
  - Added background pagination state (`backgroundLoading`, `backgroundError`, `backgroundTimer`, `listGeneration`).
  - Added `scheduleBackgroundLoad(...)` and generation guards so stale background responses from older filters/refreshes are ignored.
  - Extended `fetchList(...)` with a `background` mode that fetches append pages without setting the blocking `loadingMore` affordance.
  - Automatically schedules additional background pages after initial first paint and after each successful background page until all rows are loaded.
  - Keeps manual `Load more` as an explicit fallback/retry path, while disabling it only during an active background request.
  - Shows lightweight header/list status while remaining summaries are being loaded or when background loading pauses.
- Updated `crates/caco-web/static/summaries.css` for the new background-loading status row.
- Added source contract test `summaries_background_loads_remaining_pages_bd_2dd008` in `crates/caco-web/src/tests.rs`.
- Updated `SPEC.md` session-summary viewer UX contract to require post-first-paint background page fetching/progressive append in the web viewer.

## Validation

- `node --check crates/caco-web/static/summaries.js` — passed.
- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-web/src/tests.rs` — passed.
- `git diff --check` — passed.
- `cargo test -p caco-web summaries_background_loads_remaining_pages_bd_2dd008 -- --test-threads=1` — passed.
- `cargo clippy -p caco-web --lib --no-deps -- -D warnings` — passed.

## Notes

- This is a focused web-client slice. The existing daemon paginated `/api/v1/summaries` endpoint remains unchanged; the web client now consumes it progressively after the critical first batch.

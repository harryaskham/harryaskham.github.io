# Session summary — bd-aa0724 slice 3b: surface configured app_stores in release list response

## Goal

Complete bd-aa0724 by wiring the daemon to include the project's configured
app-store sources in the `caco release list` response, so the CLI's degraded
"pending / no record yet" rows (slice 3a) actually populate for configured
sources without a store record.

## Bead(s)

- `bd-aa0724` — caco release list/status/config: surface app-store rows (final
  slice 3b; the bead closes after this lands).

## Before state

- Failing tests: none.
- The CLI could render degraded pending rows (slice 3a), but the list response
  carried only `app_store_records`, not the configured sources, so no pending
  rows appeared.

## After state

- Failing tests: none. `cargo test -p caco-daemon --lib release` = passed
  (tj-7c830e17, exit 0): real compile verified (`Compiling caco-daemon
  v1.2.1271`, ~10 min), 74 release tests passed.
- `handle_release_list` now injects a top-level `app_stores` object (the
  project's `ProjectReleaseConfig.app_stores`) alongside `app_store_records`, so
  the CLI renders pending rows for configured-but-no-record sources. Empty →
  field omitted; source descriptors only, never a secret.

## Diff summary

- Code commits: bd-aa0724 slice 3b; final landed squash SHA from the receipt.
- Files touched: `crates/caco-daemon/src/lib.rs` (app_stores injection in the
  list response).
- Tests: covered by the caco-daemon release suite (compile + behavior); CLI
  rendering unit-tested in slice 3a.
- Behavioural delta: `caco release list` now shows pending degraded rows for
  configured Android/WearOS/iOS/watchOS sources with no record yet.

## Operator-takeaway

bd-aa0724 is complete: `caco release list` shows store records plus pending
degraded rows for configured-but-no-record sources, and `caco release config`
shows configured sources — all secret-safe (credential wrapper name only) with
unit-tested render helpers. `caco release status` stays job-scoped because store
state is project-scoped (surfaced in list). TUI/web/Android mirror via their
existing release surfaces; deeper native rendering would be separate UI beads.

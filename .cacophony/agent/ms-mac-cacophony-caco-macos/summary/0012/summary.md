# Session summary — Coalesce UI snapshot refresh bursts

## Goal

Reduce daemon pressure from multiple connected UI clients after the operator reported macOS app HTTP 500 timeouts even while the daemon was running. The focus was to add a daemon-side full-snapshot backstop, plus a macOS client throttle, so macOS, caco-web, and several TUI windows cannot all force expensive concurrent full-state reads.

## Bead(s)

- `bd-3d5f08` — [macos] throttle app full-refresh bursts that trigger daemon 30s timeouts

## Before state

- Failing tests: none assigned to this bead.
- Relevant metrics: operator-visible macOS error was `Daemon request failed with HTTP 500` and daemon body `request timed out after 30s`; local checks showed the daemon could be up while clients still timed out.
- Context: `DaemonState.refresh()` in the macOS app fanned out many concurrent endpoint requests, and stream events could trigger repeated broad refreshes; daemon `/api/v1/ui/snapshot` had a short fresh cache but no single-flight rebuild guard for expired-cache bursts.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: daemon snapshot cache now has a single-flight rebuild guard and bounded stale-while-rebuild serving; macOS app refresh now coalesces overlapping refresh requests and debounces SSE-triggered full refreshes.
- Context: multiple UI clients should now receive cached or bounded-stale snapshots while one request rebuilds the expensive full state, reducing request pile-ups and 30s timeout surfaces.

## Diff summary

- Commits: `ddddf0c52`
- Files touched: `crates/caco-daemon/src/ui_stream.rs`, `companion/macos/Sources/Cacophony/App/DaemonState.swift`, `companion/macos/Sources/CacophonyKitSmoke/main.swift`, `SPEC.md`.
- Tests: added daemon cache tests for bounded stale bytes and single-flight rebuild guard; added macOS smoke source assertions for refresh coalescing.
- Behavioural delta: UI snapshot rebuilds are coalesced daemon-side, followers get bounded stale responses instead of starting more expensive work, and the macOS app keeps at most one active full refresh plus one queued catch-up refresh.

## Operator-takeaway

The fix treats full-state refresh as shared daemon work rather than per-client work: one rebuild feeds many clients, so opening macOS, web, and multiple TUI windows should no longer multiply snapshot load into daemon timeouts.

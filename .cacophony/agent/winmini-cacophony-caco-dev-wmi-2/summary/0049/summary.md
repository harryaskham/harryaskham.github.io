# Session summary — TUI snapshot cache can recover from stale fully-fresh state

## Goal

Fix the ms-mac TUI snapshot-cache recovery path after the operator confirmed that the old cache problem was not just a missing file: a weeks-old fully-fresh `ui-snapshot-cache.json` had been trusted ahead of current live state until it was manually removed. The goal was to make the TUI persist usable live snapshots when there is no recent good cache, while still protecting genuinely recent fully-fresh caches from degraded replacements.

## Bead(s)

- `bd-303941` — Fix ms-mac TUI full UI snapshot endpoint returning empty reply and leaving stale May 7 cache

## Before state

- Failing tests: none known for this cache gate before changes.
- Relevant metrics: ms-mac initially reproduced `GET /api/v1/ui/snapshot` as a curl empty reply while `/api/v1/node`, `/api/v1/projects`, and `/api/v1/agents` remained healthy. After restart, ctrl reported live snapshots worked but no new `ui-snapshot-cache.json` existed, and Harry clarified a manually deleted old fully-fresh cache from about 20 days earlier had been masking current live state.
- Context: the TUI only persisted fully-fresh snapshots (`freshness.beads == fresh` and `freshness.agents == fresh`) and only accepted fully-fresh persisted snapshots for warm-start. Degraded-but-usable live snapshots could display current state but leave no persisted fallback, or avoid replacing an arbitrarily old fully-fresh cache.

## After state

- Failing tests: none in focused queued validation. One earlier queued test attempt failed with retryable disk exhaustion; first-party `caco prune run --cargo-targets --current-agent` freed 9.1 GiB before retry.
- Relevant metrics: default stale-cache age gate is 48 hours via `tui.ui_snapshot_cache_max_age_hours`.
- Context: degraded-but-usable snapshots containing project, agent, bead, or persistent-agent rows are persisted when there is no cache or when the existing fully-fresh cache is older than the configured max age. Recent fully-fresh caches remain protected from degraded replacement. Warm-start accepts these usable degraded caches and marks their freshness stale on reuse.

## Diff summary

- Code/content commits: `7a08d7f11` (`bd-303941: persist usable degraded tui snapshots`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-config/src/model.rs`, `SPEC.md`, `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/pending/summary.md`
- Tests: +3 focused TUI cache tests; config schema field coverage updated for the new `tui.ui_snapshot_cache_max_age_hours` field.
- Validation:
  - `git diff --check`
  - queued `cargo test -p caco-tui --lib app::tests::degraded_snapshot_without_existing_cache_is_persisted_bd_303941 -- --exact` — job `tj-e2689833` failed with retryable disk exhaustion
  - `caco prune run --cargo-targets --current-agent --json` — freed 9.1 GiB
  - queued `cargo test -p caco-tui --lib app::tests::degraded_snapshot_without_existing_cache_is_persisted_bd_303941 -- --exact` — job `tj-1df7bab6` passed
  - queued `cargo test -p caco-tui --lib app::tests::warm_start_accepts_degraded_but_usable_snapshot_cache_bd_303941 -- --exact` — job `tj-9d443004` passed
  - queued `cargo test -p caco-tui --lib app::tests::degraded_snapshot_replaces_old_fully_fresh_cache_after_configured_age_bd_303941 -- --exact` — job `tj-3bbc433a` passed
  - queued `cargo test -p caco-tui --lib app::tests::degraded_snapshot_merges_live_project_inventory_and_does_not_overwrite_cache -- --exact` — job `tj-ace2d1d7` passed
  - queued `cargo test -p caco-config --lib config_schema_field_completeness` — job `tj-e615f281` passed after an earlier exact-filter zero-test attempt (`tj-d64c79a4`)
- Behavioural delta: a 20-day-old fully-fresh cache no longer wins forever over a current degraded-but-usable snapshot; operators can tune the threshold under `tui.ui_snapshot_cache_max_age_hours`.

## Operator-takeaway

The fix is deliberately conservative: recent fully-fresh caches still protect the UI from degraded live data, but an old “fully fresh” cache is treated as stale evidence after 48 hours by default. That matches Harry's observed failure mode where a weeks-old cache looked authoritative enough to keep the TUI stuck in old state.

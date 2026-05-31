# Session summary — green up caco-web after app.js shard split

## Goal

Restore `cargo test-small` to green by fixing the 11 broken-on-main caco-web
dashboard tests that went stale after `app.js` was split into separate embedded
JS shards (`nodes.js`, `summaries.js`) and after related UI reworks. The bead
was filed as "just update which asset the tests scan", but the breakage was
broader: some assertions pinned display copy and DOM markup that legitimately
changed, and one test pinned a feature that was intentionally removed.

## Bead(s)

- `bd-7c2c80` — [broken-on-main] caco-web: 11 app_js_* tests stale after app.js->nodes.js split (f830d73953)

## Before state

- Failing tests (caco-web `--lib`, 11): `app_js_labels_snapshot_proxy_timeout_as_degraded_bd_d78de9`,
  `app_js_nodes_view_has_master_detail_and_node_scoped_chat_bd_5f3d5c`,
  `app_js_nodes_view_treats_macos_load_as_informational_bd_eb9999`,
  `app_js_preserves_last_known_data_on_sse_disconnect_bd_51e2e2`,
  `app_js_surfaces_backend_unavailable_for_snapshot_5xx_bd_c3521a`,
  `app_js_uses_api_unwrap_helper_for_success_envelope_sites`,
  `mobile_agents_table_hides_secondary_columns_bd_d3a96e`,
  `mobile_beads_table_hides_secondary_columns_bd_e0c728`,
  `mobile_services_table_hides_secondary_columns_bd_55c6bf`,
  `summaries_background_loads_remaining_pages_bd_2dd008`,
  `summaries_loading_validates_and_retries_failures_bd_37188a`.
- caco-web lib: 489 passed / 11 failed.

## After state

- Failing tests: none. caco-web `--lib` (queued, `--test-threads=2`): 500 passed / 0 failed.
- All assertions now target the current shard, current UI copy, current DOM
  markup, or the intentional pagination behaviour.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-web/src/tests.rs` only.
- Tests: 11 retargeted (0 added / 0 deleted); behaviour preserved, brittleness reduced.
- Key changes:
  - Added `dashboard_js_combined()` helper concatenating `app.js + nodes.js +
    summaries.js`; node/apiUnwrap/macOS-loadavg marker tests scan the combined
    surface so future shard splits don't re-break them.
  - `apiUnwrap` node-fetch window now read from `nodes.js` (it moved there).
  - Connection tests (`d78de9`, `c3521a`, `51e2e2`): updated pinned display copy
    to current compact glyph form (`◌ Snapshot`, `⚠ Backend`) and reworded
    diagnostics; dropped two removed cached-copy literals; durable status/state
    invariants retained.
  - Mobile tests (`d3a96e`, `e0c728`, `55c6bf`): static `<th>` now carries
    `scope="col"` (a11y); JS-rendered header assertions de-brittled to
    `class=...` + `onclick=...` substrings instead of an exact concatenation
    that broke when a `${sa(...)}` sort-attr was inserted.
  - `summaries_background_loads_remaining_pages_bd_2dd008`: bd-f04e8e (closed
    feature) intentionally replaced background/infinite scroll with pagination,
    so this test now asserts the pagination invariant and guards that
    `scheduleBackgroundLoad`/`backgroundLoading` stay removed.
  - `summaries_..._bd_37188a`: aria-busy needle updated to the current single
    `STATE.loading` form.
- Behavioural delta: none (test-only change).

## Operator-takeaway

These app_js_* content-presence tests recur as broken-on-main every time a
caco-web JS shard is split or UI copy is reworded, because they pin exact
substrings against one named asset. The new `dashboard_js_combined()` helper and
the de-brittled markup assertions reduce that churn class. One test was pinning
a deliberately-removed feature (summaries background loading, replaced by
pagination in bd-f04e8e two weeks ago) — worth watching for similar "test
outlived the feature" cases when greening up after large reworks.

# Session summary — bd-c0ebb6: FEED-pane render cache (TUI keystroke input-lag, P0)

## Goal

Fix the sluggish TUI keystroke latency under large frame areas (the P0 ms-dev-ctrl designated me the fresh hand for, from msd-2's complete spec). The attribution was already solved (the per-keystroke cost is the full-frame re-BUILD of every pane, static ones included); the job was to implement the per-pane render cache that blits an unchanged pane instead of rebuilding it, with the two named render-cache reviewers (msd-2 + tui-md2-1) reviewing the diff.

## Bead(s)

- `bd-c0ebb6` — Fix sluggish TUI keystroke and mouse input during pane updates (P0). Reviewers: `caco-dev-msd-2` + `caco-tui-md2-1`.

## Before state

- Failing tests: none. Baseline (installed 1.2.1352, default overview-agents scene): avg 6.47ms / p95 8.24 / p99 9.69 / slow16=1 @500x140.
- `render_content_for` called `render_content_in_area` every frame for every tile; the feed's ANSI-parse/wrap content build re-ran every keystroke even when unchanged.
- The 500x140 keystroke-benchmark only rendered `ProjectAgentsView` as the non-composer pane (scene[0]=OverviewAgents + a GlobalInbox focus override), so a feed-only cache would have shown zero delta there.

## After state

- Failing tests: none. `cargo check -p caco` green; `cargo test -p caco-tui --lib` green; `cargo clippy -p caco-tui -p caco-cli -- -D warnings` clean.
- A per-tile rendered-buffer cache for the FEED pane blits the unchanged feed instead of rebuilding it. The benchmark gained a `--scene` knob so the feed can be the measured non-composer pane.
- PROOF (500x140, release, `--scene feed-logs --no-background-paint`): BEFORE (cache off) avg 9.46 / p95 12.63 / p99 15.97 / max 27.34 / slow16=3; AFTER (cache on) avg 6.16 / p95 8.12 / p99 10.68 / max 11.92 / slow16=0. → avg −35%, p99 −33%, slow_keystrokes_16ms 3→0. No-regression: overview-agents (cache on) avg 5.80 / slow16=0.

## Diff summary

- Code/content commit: `7335a31b8d` (pending final squash SHA from the reintegration receipt).
- Summary artefact commit: intentionally omitted (must not self-reference its own mutable SHA).
- Files touched: `crates/caco-tui/src/app.rs` (App `feed_render_cache` + `feed_render_cache_enabled` fields; `snapshot_buffer_area`/`blit_buffer_area`/`feed_render_theme_signature`/`feed_render_cache_hash` assoc fns; the cache wrap in `render_content_for`; resize-clear), `crates/caco-tui/src/app/benchmark_support.rs` (`--scene` selector: config field + result field + `BenchmarkScene::from_label` + scene-selection with typo-reject), `crates/caco-cli/src/lib.rs` (dispatch + `--scene` ArgSpec).
- Tests: +0 new (the cache is a render-equivalence optimization; the benchmark before/after is the proof). The benchmark's existing `keystroke_latency` test still passes with the added `scene` field.
- Behavioural delta: on a composer-only keystroke the feed tile blits its cached cells instead of rebuilding; non-feed panes and the kitty-surface lifecycle are unchanged.

## Embedded artefacts

- None (headless textmode benchmark; the before/after JSON is inline above).

## Operator-takeaway

The per-keystroke TUI lag at large frame areas was the full-frame pane REBUILD, and the highest-leverage safe first slice is caching the FEED pane — uniquely safe because its content_tag is already a render-complete data signal (entries+scroll+selection+anchor+filter), so a cache hit can't serve a stale frame. The same cache is NOT safe for panes whose lifecycle tag omits row data (e.g. ProjectAgentsView) — and crucially the static-data benchmark would not catch that stale blit, so those panes need a real data-inclusive hash as a deliberate follow-up (path (b)). The benchmark also needed a `--scene feed-logs` knob because its default scene never rendered the feed. Net: −35% avg / slow16→0 on the feed scene, env-toggleable (`CACO_TUI_FEED_RENDER_CACHE=0`) for rollback.

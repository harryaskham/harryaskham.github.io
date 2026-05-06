# bd-9b7b54: skip background preflight when config absent but cache stale

## What changed

- Tightened the background branch in `App::flush_graphics_requests()` to run only when `graphics_config.is_some()`.
- If graphics config is absent, per-request background preflight is skipped even when stale app background cache roots exist.
- Existing inactive-root pruning still clears stale background cache entries because no active roots are marked in the no-config case.
- Updated source-shape coverage for both the no-config/no-cache and no-config/stale-cache paths.

## Why

When no graphics config exists, no background can render. The previous guard still entered per-request background preflight when stale background cache roots existed, even though inactive-root pruning can remove those roots without resolving config or formatting root keys. Skipping the branch removes unnecessary per-panel work during no-config frames with stale background cache state.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_9b7b54"` — `tj-d38a595a`, passed

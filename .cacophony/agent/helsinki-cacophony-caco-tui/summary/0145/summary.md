# bd-ee7f93: skip false-positive background preflight render path

## What changed

- `flush_graphics_requests()` now uses the resolved `background_layers_renderable` flag as a guard before formatting the background root key, constructing an effective background request, doing app-cache lookup, or calling the background render helper.
- Conservative raw-config preflight can still admit possibly-renderable requests, but if resolved layers prove all-`None`, the path stops there.
- Stale cache cleanup remains covered by inactive-root pruning: non-renderable panels do not mark active roots, so old entries are pruned at the end of the flush pass.
- Added source-shape coverage to ensure root-key/cache/render work remains behind the resolved renderability guard.

## Why

The raw preflight intentionally errs on the side of possible rendering. Before this change, false positives still formatted keys and called into the render helper just to return `None`. The resolved layer scan already knows whether anything can render, so using that result avoids unnecessary background cache/render work while preserving correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_ee7f93"` — `tj-a6c15f8d`, passed

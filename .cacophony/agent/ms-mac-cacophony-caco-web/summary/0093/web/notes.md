# caco-web duty cycle notes — 0093 / bd-8a6877

- Rebased through a transient `.git/index.lock` first-party rebase failure; lock was gone on inspection and `caco agent rebase` succeeded on retry.
- Inbox confirmed the controller decision that `bd-9e4be4` remains solely owned by `yuyg5sygj4ums1fj`; caco-web acknowledged and did not touch that implementation.
- Initial board scan found no assigned caco-web bead and no ready/open web-adjacent bead; `bd-1cf76a` remained in progress under ms-dev.
- Current-assets `caco-web-observe` showed the snapshot-timeout state now handled Status, Feed, and Workspace correctly, but dashboard home side cards still displayed indefinite placeholders: `RECENT ACTIVITY Loading events…` and `ACTIVE AGENTS Loading agents…`.
- Filed and explicitly claimed `bd-8a6877 — caco-web dashboard side cards stay loading during snapshot timeout` after bead service recovered enough to confirm no ready/open duplicate.
- Implemented the fix in `crates/caco-web/static/app.js`:
  - `renderRecentEvents()` now detects initial unavailable snapshot data and renders `Recent activity unavailable` / delayed copy instead of `Loading events…` / healthy empty copy.
  - `renderActiveAgentsSummary()` now detects initial unavailable snapshot data and renders `Active agents unavailable` / delayed copy instead of `Loading agents…` / healthy empty copy.
  - `setConnectionStatus()` now rerenders those Status side cards when the connection transitions into the initial snapshot-unavailable state, so the cards update even when no usable snapshot ever applies.
- Added static contract coverage in `crates/caco-web/src/tests.rs`: `app_js_dashboard_side_cards_respect_snapshot_timeout_bd_8a6877`.
- Validation passed: `cargo fmt --all -- --check`, focused bd-8a6877 test, bd-2418f5 regression test, and `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`.
- First after-fix browser observation exposed the missing render trigger; the second after-fix observation confirmed home side-card copy changed to `Recent activity unavailable` and `Active agents unavailable`.
- Reflect-session filed draft `bd-d5360c` for DOM-level caco-web snapshot-timeout tests because the initial string-only test missed the render-trigger gap.

# Session summary — workspace.html links every embedded pane CSS shard

## Goal

Make the 5 unstyled workspace panes actually render with their
authored layout. Five (six counting `workspace-dnd`) shards were
embedded in the binary but never linked from `workspace.html`, so
`.workspace-chat-pane` / `.workspace-log-pane` / etc. rendered with
default browser styling. Embedded ≠ loaded.

## Bead(s)

- `bd-d59265` — `[caco-web] workspace.html missing <link> tags for
  5 embedded pane CSS shards (panes likely render unstyled)` (P1 bug)

## Before state

- Failing tests: none (the per-shard `*_css_is_embedded` tests
  asserted embedding, never linkage; they passed despite the panes
  being unstyled).
- `workspace.html` linked only `vendor/xterm`, `workspace-a11y.css`,
  `workspace-mobile.css`, `workspace.css`.
- Five workspace-pane shards + one `workspace-dnd` shard sat in
  `static/` and got embedded by rust-embed but were never fetched.

## After state

- `workspace.html` now links 6 additional pane shards in the order
  documented in `static/README.md` (vendor → tokens → a11y →
  view/pane shards): `workspace-bead-detail.css`,
  `workspace-bead-list-pane.css`, `workspace-chat-pane.css`,
  `workspace-log-pane.css`, `workspace-stt-mic.css`,
  `workspace-dnd.css`.
- New `workspace_html_links_every_workspace_css_shard_bd_d59265`
  test iterates the rust-embed asset list at runtime, filters to
  `workspace-*.css`, and asserts each is referenced by an
  `href="/<file>.css"` link tag. Empty allowlist documents the
  current zero-exception policy.
- Future-proof: any new `static/workspace-foo.css` added without a
  matching link will fail this test loudly — the regression that
  shipped 5 shards in a row without anyone noticing cannot recur
  silently.
- `cargo test-small` 176/176 green (up from 162; the inventory grew).

## Diff summary

- Files touched:
  - `crates/caco-web/static/workspace.html` (+6 link tags + a doc
    comment pointing at the README load-order rule and the new
    guard test)
  - `crates/caco-web/src/tests.rs` (+1 inventory-driven test)
- Tests: +1 / -0 / flipped 0
- Behavioural delta: workspace panes now render with their authored
  CSS instead of falling back to global tokens.

## Operator-takeaway

The reason this hid for so long is that "shard exists" and "shard
loads" were the same green light in the test suite. The new guard
test closes that gap by treating the rust-embed inventory as the
source of truth and the HTML as the contract. Future caco-web bead
authors can add a pane shard with confidence — if they forget the
`<link>`, CI tells them.

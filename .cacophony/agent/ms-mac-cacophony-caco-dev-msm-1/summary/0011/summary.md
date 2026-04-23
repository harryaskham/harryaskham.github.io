# Session 0011 — bd-4431dc cycle 1: workspace-view contract tests

## Outcome
Cycle 1 of the PERMANENT bd-4431dc (workspace-view ongoing testing).
Added 6 contract-locking tests in `crates/caco-web/src/tests.rs` so
MVP (bd-a78749) and the parallel beads can build against a single,
test-enforced spec.

## Context
caco-ctrl filed the workspace-view epic bd-027e9d (13 beads).
By time I started, several parallels had already landed (bd-232e03
pane-tree, bd-400e1a responsive, bd-1328dd detail, bd-09f314 a11y).
Took bd-4431dc since it's explicitly designed for repeated cycles and
benefits from contract-pinning even before MVP lands.

## Commit
- `cddfb437` — bd-4431dc cycle 1: workspace-view contract tests
  (+181 lines in crates/caco-web/src/tests.rs).

## Tests added (all suffixed `_bd4431dc` to coexist with parallel landings)
1. `workspace_pty_ws_endpoint_is_registered_for_workspace_panes` —
   locks `/api/v1/agents/<id>/pty/stream` route in `server.rs`.
2. `workspace_terminal_honours_pty_readonly_contract_bd4431dc` —
   duplicates bd-6c75d6 hello-frame wiring so a rename there doesn't
   silently break bd-a40535 (terminal pane).
3. `workspace_split_ratio_clamp_invariant_bd4431dc` — 1000-iter
   xorshift fuzz; idempotent, in-bounds [0.05, 0.95]. Same formula
   pane-tree (bd-232e03) already inlined.
4. `workspace_saved_view_shape_contract_bd4431dc` — pins v0
   saved-view JSON shape + pane-kind set
   (bead_list/terminal/chat/log/bead_detail) for bd-fdc5f5.
5. `workspace_localstorage_key_surface_documented_bd4431dc` —
   canonical `workspace.*` key list for cross-bead alignment.
6. `workspace_mvp_route_and_static_scaffolding_present_bd4431dc`
   (`#[ignore]`) — acceptance gate for bd-a78749 MVP.

## Tests results
- `cargo test -p caco-web workspace_`: 13 passed (includes
  bd-232e03/bd-400e1a tests already landed), 1 ignored, 0 failed.
- `cargo clippy -p caco-web`: clean.

## Decisions
- **Suffix tests with bead-id**: `_bd4431dc` suffix prevents naming
  collisions with parallel landings. Future cycles can dedupe by
  promoting overlapping checks to a shared helper.
- **Test as spec, not as coverage**: primary value here is locking
  contracts that 9 parallel PRs would otherwise re-derive and drift
  on. Where the live surface doesn't exist (saved-view JSON), the
  test body documents the minimum expected shape.
- **Contract duplication over reference**: duplicated read-only
  hello-frame assertion rather than calling the bd-6c75d6 test, so
  renaming there doesn't silently remove the workspace contract.

## Open / next
- bd-4431dc remains PERMANENT (claimable for next cycle).
- Future cycles: unignore MVP acceptance test once bd-a78749 lands;
  add browser-side drag-resize fuzz; add pane stress tests; add
  saved-view round-trip test against the live API once bd-fdc5f5
  ships.

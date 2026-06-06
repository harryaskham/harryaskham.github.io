# Session summary — bd-210bdf hook mixin ordering fix

## Goal

Fix the caco-profile `merge_hooks` ordering bug so hook mixins apply in the documented base/list order, with inline hooks layered after mixin hooks.

## Bead(s)

- `bd-210bdf` — `[profile] merge_hooks swaps base/overlay args, inverting documented overlay-on-top hook order`

## Before state

- `crates/caco-profile/src/hook_mixins.rs` merged per-phase hooks as `overlay ++ base`, reversing mixin list order and contradicting `resolve_and_merge` / `Profile` model docs.
- The existing `merge_hooks_combines_phases` test only checked count and contained comments noticing the confusion but did not pin order.

## After state

- `merge_hooks` now calls `merge_phase(base, overlay)` for every hook phase, so existing/base hooks run before overlay hooks.
- Tests assert the exact order for base + overlay and for inline hooks layered on top.
- Targeted validation passed: `tj-3f07d4de` (`cargo test -p caco-profile hook_mixins -- --test-threads=2`).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-profile/src/hook_mixins.rs`
  - `.cacophony/agent/aurora-cacophony-caco-dev-aur-2/summary/pending/summary.md`
- Tests: strengthened hook mixin tests with explicit order assertions; added inline-on-top order coverage.
- Behavioural delta: later hook mixins and inline hooks now append after earlier/base hooks, matching documented overlay semantics.

## Operator-takeaway

The code path was quietly reversing hook execution order while the docs said list order. The fix makes the behavior match the docs and pins it with order assertions so future mixin changes cannot accidentally reintroduce the inversion.

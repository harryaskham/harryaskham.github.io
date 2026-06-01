# Session summary — Fix caco-tui fast-test-gate flake (bd-da1807)

## Goal

While evaluating aur-2's bd-2fca0c (panel_tabs graphics) handoff, aur-2 warned of
"8 failing graphics tests on main" making caco-tui landings unreliable. I
investigated that claim before reaching for bd-2fca0c.

## Finding

It was NOT 8 failing tests — it was exactly ONE flaky test:
`app::tests::project_overlay_updates_graphics_active_before_first_frame`. It passes
in isolation (`--test-threads=1 --exact`) but fails under `--test-threads>=2`
(329 passed / 1 failed). Because `test-small` runs the caco-tui lib suite with
`--test-threads=2`, this flake can intermittently fail the `cacophony-fast-tests`
reintegration gate for ANY caco-tui change — the real instability behind the
"can't land caco-tui work" reports.

## Root cause

`graphics_active()` reads a process-global `GRAPHICS_ACTIVE: AtomicBool` unless a
thread-local `GRAPHICS_ACTIVE_OVERRIDE` is set (bd-f7a076). The failing test seeded
and asserted against the GLOBAL atomic across an `app.render()` (which internally
calls `set_graphics_active(false)` on the global). A parallel test toggling the
shared global between this test's render and assertion caused the flake.

## Fix

- `common::set_graphics_active` now writes through to the thread-local override
  when one is active (no-op in production, which never sets an override).
- The test establishes `set_graphics_active_override(true)` before rendering, so
  render's write-through keeps the value thread-local and the assertion is immune
  to parallel global writes. Existing `clear_graphics_active_override()` cleanup
  unchanged.

## Bead(s)

- `bd-da1807` — filed + claimed + fixed this session (broken-on-main flake).
- Related/unblocked: `bd-2fca0c` (panel_tabs state overrides, still open) becomes
  cleanly landable once this flake is off main; left for aur-2/graphics owner.

## Diff summary

- Code commits: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-tui/src/views/common.rs` (write-through), `crates/caco-tui/src/app.rs` (test isolation).
- Verification: isolated pass; graphics suite `--test-threads=2` 330/330 stable
  across 3 runs; full caco-tui lib `--test-threads=2` 4079 passed / 0 failed;
  `cargo clippy -p caco-tui --lib` clean. caco-tui-only → lands on aurora's 300s gate.

## Operator-takeaway

The intermittent caco-tui reintegration-gate failures were caused by a single
test-isolation flake racing on a shared graphics-active global, not by broken
product code. Fixed; caco-tui landings are stable again.

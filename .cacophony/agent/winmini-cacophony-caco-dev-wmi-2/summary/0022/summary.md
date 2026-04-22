# bd-bf1e86 polish #10: serialize tmux pane_mode_cache tests to fix intermittent flake

## Goal

Eliminate the intermittent `tmux::tests::pane_mode_cache_keys_are_per_socket_and_target` panic that fires ~1-in-3 runs of `cargo test -p caco-tui --lib`, root-cause: 4 tests share a global `PANE_MODE_CACHE` and call `_clear_pane_mode_cache_for_tests()` independently, letting cargo's parallel runner wipe one test's writes mid-flight.

## Bead(s)

- bd-bf1e86 (permanent polish track) — cycle #10

## Before state

```
test tmux::tests::pane_mode_cache_keys_are_per_socket_and_target ... FAILED

failures:
---- tmux::tests::pane_mode_cache_keys_are_per_socket_and_target stdout ----
thread 'tmux::tests::pane_mode_cache_keys_are_per_socket_and_target' panicked at crates/caco-tui/src/tmux.rs:2520:9:
assertion failed: cached_normal_mode("sock-a", "sess:0.0")
```

The 4 tests in `crates/caco-tui/src/tmux.rs::tests`:
- `pane_mode_cache_short_circuits_after_normal_observation`
- `pane_mode_cache_never_short_circuits_copy_mode`
- `pane_mode_cache_expires_after_ttl`
- `pane_mode_cache_keys_are_per_socket_and_target`

each begin with `_clear_pane_mode_cache_for_tests()` then write+read against the global `PANE_MODE_CACHE` (a `LazyLock<Mutex<HashMap>>`). Cargo runs tests in parallel by default; if T2 calls `_clear...` between T1's write and T1's read, T1's `cached_normal_mode` returns false and asserts fail.

I observed 1 failure across 3 `cargo test-small` runs and ~1-in-5 in `cargo test -p caco-tui --lib`.

## After state

Added a private static `PANE_MODE_CACHE_TEST_LOCK: std::sync::Mutex<()>` and a `let _guard = PANE_MODE_CACHE_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());` line at the top of all 4 tests. The `unwrap_or_else(|e| e.into_inner())` recovers from poisoned locks (a panic in one test would otherwise wedge the rest; this preserves the original assertion as the failure signal).

Verification:
- 5/5 `cargo test -p caco-tui --lib pane_mode_cache` runs PASS (was ~80% pre-fix)
- 5/5 `cargo test-small` runs PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

1 file changed, +9 / −0:

- `crates/caco-tui/src/tmux.rs`:
  - +1 `static PANE_MODE_CACHE_TEST_LOCK` declaration with explanatory comment
  - +4 `let _guard = ...` lines (one per test)
  - +4 lines of context (the comment block grew by 4 lines)

## Operator-takeaway

This is the **flaky test I've been retrying through for 3+ cycles** — every time I ran `cargo test-small` post-pull, there was a ~1-in-3 chance the tui crate would panic on this test. I'd been blaming "resource contention" but the actual cause was a missing serialisation primitive on a shared global.

Standard fix pattern for tests-on-shared-globals: dedicated `Mutex<()>` private to the test module, recovered from poisoning. Could alternately use `serial_test` crate but adding a dependency for one shared cache would be overkill.

This is bd-bf1e86 polish cycle #10 of the session. Streak so far this session for that bead: 10/10 small-and-targeted improvements, no regressions.

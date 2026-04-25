# Session summary — config-show test env isolation

## Goal

Fix the `caco-cli` `config_show_*` tests that sat behind the global process-env mutex for over 60 seconds during the full parallel library suite. The goal was to keep these tests isolated from unrelated env-mutating siblings by using the existing `--config` override path instead of mutating `CACOPHONY_DIR` and `CACO_NODE`.

## Bead(s)

- `bd-d2e8f4` — caco-cli config_show tests hang for >60s under parallel execution

## Before state

- Targeted `timeout 180 cargo test -p caco-cli --lib config_show -- --nocapture` passed: 17/17.
- Full `timeout 420 cargo test -p caco-cli --lib -- --nocapture` showed `config_show_dispatches_json`, `config_show_dispatches_yaml`, and `config_show_node_filter_*` running for over 60 seconds while blocked behind other tests using the shared `ENV_MUTEX`.
- The same full run also exposed unrelated later failures/timeouts outside this bead scope.

## After state

- `config_show_*` tests use a per-test temporary `config.yaml` passed through `dispatch_config_show(..., Some(&config_path))`, avoiding process-env mutation entirely.
- Adjacent `config_validate_strict*` tests using the same helper now also pass their config override explicitly.
- In the next full `timeout 420 cargo test -p caco-cli --lib` run, all `config_show_*` tests completed and passed instead of hanging; the run later timed out on unrelated tests, tracked separately.

## Diff summary

- Commits: `ea2e2e246`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: no new tests; rewired existing config-show/config-validate tests to avoid env mutation.
- Behavioural delta: test-only change. The CLI production path is unchanged; tests now exercise the same `--config` override path operators can use directly.
- Validation: `timeout 180 cargo test -p caco-cli --lib config_show`, `timeout 120 cargo test -p caco-cli --lib config_validate_strict`, `timeout 120 cargo test -p caco-cli --lib at_all_missing_command`, `timeout 120 cargo test-small`.

## Operator-takeaway

The config-show hang was not a product deadlock; it was test harness contention from using global env mutation for tests that already had a cleaner config-override path. The affected tests now finish quickly under the full suite. Remaining full-suite failures were filed separately as `bd-7f6df9`.

# Session summary — Expand test_queue + build_queue unit coverage (bd-237432)

## Goal

bd-237432: caco-daemon's test_queue.rs (1127L) and build_queue.rs
(960L) implement SPEC §21.2/21.3 queued validation/build surfaces.
Combined ~2087 lines but only 27 unit tests — none covering
cancel-running, list filtering/sort, autodetect priority,
front-door zero-timeout rejection, ID uniqueness, or terminal-state
errors. Add focused tests for these gaps.

## Bead(s)

- `bd-237432` — Expand test_queue and build_queue test coverage
  (1750 lines, 27 tests)

## Before state

- caco-daemon --lib failed to compile due to a peer's bd-7ef076
  follow-up adding `tmux_history_limit` / `tmux_history_size` to
  `AgentSnapshot` without updating 3 test fixtures.
- test_queue::tests: 16 passing.
- build_queue::tests: 14 passing.

## After state

- caco-daemon --lib compiles cleanly; ui_stream.rs fixtures
  backfilled with tmux_history_* fields.
- test_queue::tests: 30 passing (+14).
- build_queue::tests: 25 passing (+11).
- Total queue-module tests: 27 → 55 (+25 new tests).

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/test_queue.rs` (+~265): 14 new tests
  - `crates/caco-daemon/src/build_queue.rs` (+~225): 11 new tests
    (build queue lacks per-request max_runtime override, so the
    matching zero-timeout test is intentionally absent)
  - `crates/caco-daemon/src/ui_stream.rs` (+6): drive-by fix for
    pre-existing AgentSnapshot field omission
- Tests: +25 / -0 / flipped 0

### Coverage gains by surface

**Cancel state machine:**
- cancel_running_marks_was_running_true (both modules)
- cancel_terminal_job_returns_error (both modules)
- cancel_unknown_job_returns_error (both modules)

**List semantics:**
- list_filters_by_state_and_respects_limit (both modules)
- list_sorts_queued_first_then_terminal_by_queued_at_desc
  (test_queue — pins the visible-to-operator ordering rule)

**Lookup contracts:**
- get_unknown_id_returns_none (both modules)
- logs_unknown_id_returns_error (both modules)

**ID generation:**
- generate_job_id_is_unique_across_counters (both modules,
  prefix-checked: tj- for test, bj- for build)

**Autodetect priority chain (SPEC §6.5.4.2):**
- autodetect_priority_cargo_over_npm (both modules)
- autodetect_makefile_with_test_target (test_queue)
- autodetect_makefile_without_test/build_target_returns_none
  (both — defensive None when target missing)

**Enqueue precedence:**
- enqueue_request_command_overrides_config_command (both)
- enqueue_rejects_zero_max_runtime (test_queue — front-door)

**Display contract (CLI/JSON output stability):**
- test_job_state_display_stable_lowercase (test_queue)
- build_job_state_display_stable_lowercase (build_queue —
  documents the divergence: build uses `Succeeded`, test uses
  `Passed`)

## Embedded artefacts

(none — pure test additions to the queue modules)

## Operator-takeaway

The two queue modules are now defended at 55 tests instead of 27.
A future refactor that:
- silently downgrades a Running cancel to a no-op (would break
  TUI's was_running indicator),
- changes the operator-facing list sort order,
- shuffles the autodetect priority chain (e.g. accidentally
  picking `npm test` over `cargo test` in a Rust crate that also
  has a package.json),
- collides job IDs under rapid counter pressure,
- breaks the lowercase Display output that JSON envelopes ship,

will light up loudly in focused tests instead of slipping through
the broader integration suite.

The intentional API divergence between the two modules is now
documented by the parallel-but-different display test pair
(test=Passed, build=Succeeded).

Drive-by un-broke `cargo test -p caco-daemon --lib` so peers can
keep landing slices without tripping the same compile failure.

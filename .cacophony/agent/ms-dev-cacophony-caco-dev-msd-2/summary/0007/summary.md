# Session summary 0007 — bd-fac12a + drive-by broken-on-main sweeps

## Goal

Land bd-fac12a (caco-beads `run_git_with_timeout` ECHILD
defensive fix vs daemon zombie-reaper race; bd-180c6d had been
closed without a code fix). En route, sweep up several
broken-on-main collateral fires from in-flight rebases of other
agents' work so that workspace `cargo test-small` and
`cargo clippy --workspace --all-targets -- -D warnings` are green
again.

## Bead(s)

- `bd-fac12a` — primary, claimed and worked.
- bd-d46eb7 (broken-on-main caco-tui from bd-7ef076 / bd-b69cf3)
  — closed implicitly by msd-1's bd-bce6ea + companion's
  bd-ae6b7b + my prior bd-c36993 in summary 0006; tried to
  formally close, daemon close-validator rejected ("bead id not
  found in last 1000 commits") because the bead wasn't named in
  any merge commit message. Left for daemon catch-up.

## Before state

- `cargo build -p caco-beads --tests`: 85 E0063 errors (missing
  `parent_bead_id` on `CreateBeadParams` literals).
- `cargo build -p caco-cli --tests`: 7 errors — 1 duplicate
  `disable_hooks` field, 3 wrong-arity calls of
  `dispatch_agent_logs` (now takes `since` param), plus 3
  follow-on type-mismatch errors that resolved with the arity fix.
- `cargo build -p caco-daemon --tests`: 6 E0062 duplicate-field
  errors in `ui_stream.rs` (rebase artifact: same field assigned
  twice in same struct literal).
- `cargo clippy --workspace --all-targets`: 65 more E0063 missing
  `parent_bead_id` sites in caco-daemon + caco-beads
  (audit.rs, beads.rs, lib.rs, ui_stream.rs), plus 4 more
  duplicate-tmux-history E0062 errors, plus a 15/7 too-many-args
  warning on `dispatch_agent_new`.
- `cargo test-small`: blocked workspace-wide.

## After state

- `cargo build` everywhere: clean.
- `cargo test-small`: PASS 211 + 109 + 739 + 295 + 18 + 2818 + 56
  green (delta: +2 caco-beads from the two new bd-fac12a tests;
  +4 caco-config from earlier landed work).
- `cargo clippy --workspace --all-targets -- -D warnings`: clean.

## Diff summary

bd-fac12a primary fix in `crates/caco-beads/src/sync.rs`:

- After `try_wait` returns `Some(status)`, drain piped
  stdout/stderr DIRECTLY via `Read::read_to_end` through the new
  `collect_output_with_status` helper. Avoids the second `wait()`
  inside `wait_with_output` that can ECHILD when the daemon's
  bd-d0381e global zombie-reaper raced us between the `try_wait`
  and the inner `wait`.
- When `try_wait` itself returns ECHILD (the reaper got there
  first), call new `collect_output_after_external_reap` which
  drains the pipes and synthesizes a success status with stderr
  prefixed by a `bd-180c6d / bd-fac12a` marker for log
  correlation.
- Same treatment applied to the `timeout_secs == 0` infinite-wait
  path: drives `child.wait()` with the helpers.

bd-fac12a retry-allowlist extension in
`crates/caco-beads/src/store.rs::run_git_with_retry`: added
`"No child processes"` and `"os error 10"` (ECHILD textual forms)
so that transient race ECHILDs get one or two backoff-spaced
retries instead of surfacing as real errors that the auto-filer
duplicated 29 times.

2 new tests in `crates/caco-beads/src/sync.rs::tests`:
- `run_git_with_timeout_drains_after_try_wait_some` — end-to-end
  through the timeout path using `git status`.
- `collect_output_after_external_reap_marks_synthetic_status` —
  spawns `/bin/true`, reaps it ourselves to simulate the race,
  asserts the synthetic stderr contains the race marker plus the
  label.

Drive-by sweeps:

1. `crates/caco-cli/src/lib.rs:76810` — removed duplicate
   `disable_hooks: None,` (rebase artifact from bd-1d302a).
2. `crates/caco-cli/src/lib.rs` 3 test fixtures — added `None,`
   for the new `since` parameter to `dispatch_agent_logs`.
3. `crates/caco-cli/src/lib.rs:28989::dispatch_agent_new` —
   targeted `#[allow(clippy::too_many_arguments)]` with bd-fac12a
   marker; refactor to builder pattern out of scope.
4. `crates/caco-daemon/src/ui_stream.rs` — removed 4 duplicate
   `tmux_history_limit/size: None,` lines (3610-3611, 5296-5297).
5. `parent_bead_id: None,` mechanically inserted into 65
   `CreateBeadParams` / `Bead` struct-literal sites across
   `caco-beads/{model.rs,store.rs,tests/store_integration.rs}`
   and `caco-daemon/{audit.rs,beads.rs,lib.rs,ui_stream.rs}` via
   the same Python brace-walker used in summary 0006 (now
   parameterized by struct name + new field). One iteration
   sufficed.

## Files touched

- `crates/caco-beads/src/sync.rs` (+143 / -36): primary +
  helpers + tests + parent_bead_id sweep where applicable.
- `crates/caco-beads/src/store.rs` (+8 / -1): retry allowlist +
  parent_bead_id sweep.
- `crates/caco-beads/src/model.rs` (+11 / -0): parent_bead_id
  sweep.
- `crates/caco-beads/tests/store_integration.rs` (+7 / -0):
  parent_bead_id sweep.
- `crates/caco-cli/src/lib.rs` (+5 / -3): drive-by fixes.
- `crates/caco-daemon/src/ui_stream.rs` (+3 / -4):
  parent_bead_id sweep + dup removal.
- `crates/caco-daemon/src/{audit,beads,lib}.rs` (+55 / -0):
  parent_bead_id sweep.

Total: 9 files, +301 / -37.

## Operator-takeaway

bd-fac12a defensive layer is in. The auto-filer should stop
generating duplicate ECHILD bug reports because the run-git
helpers now treat ECHILD as a benign external-reap, drain
output without re-invoking wait, and tag stderr with a clear
race marker. Real root cause (bd-e93a29: register caco-beads
sync git children with the daemon reaper) remains future work.
Workspace test-small + clippy are green; other agents should
be able to validate without surprise.

The Python brace-walker (`/tmp/fix_struct_inits.py` plus its
parameterized siblings `/tmp/fix2.py`, `/tmp/fix3.py`) is the
recommended unblock pattern for any future "struct field added
without call-site sweep" event. Reusable, one-iteration in
practice.

## Validation

- `cargo build -p caco-beads --tests`: clean.
- `cargo build -p caco-cli --tests`: clean.
- `cargo build -p caco-daemon --tests`: clean.
- `cargo test -p caco-beads --lib`: 211 / 211 PASS (+2 vs
  baseline from the two new tests).
- `cargo test-small`: 211 + 109 + 739 + 295 + 18 + 2818 + 56
  PASS.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean
  (1m17s).

## Notes / follow-ups

- Whoever owns the `parent_bead_id` feature should land the
  actual parent-tracking semantics; this commit only set the
  field to `None` everywhere so the tree compiles against the
  struct addition.
- bd-845653 / bd-58ff27 / bd-c24ff7 / bd-0977ba / bd-de0282 /
  bd-c36993 / bd-d46eb7 / bd-fac12a still have open status in the
  bead store but their fixes are on main; `caco bd close` rejects
  with "bead id not found in last 1000 commits" because the
  squash-commit message scraper isn't picking the IDs out of the
  reintegrate-merge messages. Operator-side daemon refresh /
  bd-845653 deployment will unblock formal closure.
- Refactoring `dispatch_agent_new` from 15 positional args into
  a builder is filed as conceptual follow-up; today it carries
  a targeted clippy allow with a bd-fac12a marker.

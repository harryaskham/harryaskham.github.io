# Session summary — bd-51859d caco-cli claude_requires_project_flag fix

## Goal

Fix the `claude_requires_project_flag` test in `crates/caco-cli/src/lib.rs`,
which msm-1 surfaced as actually broken on main HEAD `7f2a17f2` (one of
the few caco-cli `--lib` failures that fails standalone — not just under
shared mutable env).

The test was supposed to assert that `caco claude` without `--project`
fails fast with an error mentioning `--project`. On a host with a
configured cacophony on disk, the test instead resolved a project via
`resolve_interactive_project`'s cascade (`default_project` /
sole-configured-project), proceeded into the daemon spawn path, and
timed out at 30s with `request timed out after 30s`.

## Bead(s)

- `bd-51859d` — fixed and closed.

## Before state

- `cargo test -p caco-cli --lib -- claude_requires_project_flag --test-threads=1`:
  ~120s wall, FAIL (`error must mention --project; got: request timed out after 30s`).
- The test only scrubbed `CACOPHONY_PROJECT` and `CACO_PROJECT` env;
  it did not isolate the on-disk config cascade.

## After state

- The test now also sets `CACOPHONY_DIR` to a fresh `tempfile::tempdir()`
  before invoking `run()`, and restores it (set/remove according to
  prior value) afterwards. This causes `load_config_for()` to find no
  `config.yaml` → `cfg = None` → none of the cascade arms fire →
  `resolve_interactive_project` returns the
  `--project is required for {runtime_type}` error fast, exactly as the
  test intended.
- `cargo test -p caco-cli --lib -- claude_requires_project_flag --test-threads=1 --nocapture`:
  PASS in 0.00s (after compile).
- `cargo test-small`: PASS (4156 tests / 7 binaries / 0 failed / 0 ignored).
- Production behaviour unchanged — only test-side env isolation tightened.

## Diff summary

- Commit: `c02dbc82`
- Files touched: `crates/caco-cli/src/lib.rs` (one test, +17 / -2).
- New helpers: none. Uses already-workspace `tempfile::tempdir()`.
- Tests: 0 added / 0 removed; 1 test now passes that previously failed.

## Out-of-scope observation

`cargo test -p caco-cli --lib` (multi-threaded, no `--test-threads=1`)
still shows 88 failures, including this test failing again — but with a
different signature (`PoisonError` on `ENV_MUTEX.lock()`). That's the
shared-env cluster msm-1 already documented in bd-51859d's "related
context" paragraph: ~83 tests pass standalone but fail under multi-threaded
shared mutable env, all caused by upstream tests panicking while holding
`ENV_MUTEX` and poisoning it for downstream tests. Fixing that cluster
needs its own bead (msm-1 noted they would file one); out of scope here.

## Operator-takeaway

`bd-51859d` as written ("falls through to 30s daemon timeout") is now
fixed. The standalone `--test-threads=1` invocation that the bead's
repro recipe uses passes cleanly. The wider env-isolation pattern that
makes the same test fail under default `cargo test` (PoisonError, not
30s timeout) is a separate root cause and a separate bead.

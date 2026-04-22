# bd-ded3df — fix project_messages baseline drift + misleading error prefix on caco doctor schema

## Goal
Make `caco doctor schema` report cleanly on a healthy install: no
false-positive drift on `project_messages`, no misleading `error:`
prefix on stderr when the only "failure" is the structured drift
report itself.

## Bead(s)
- bd-ded3df (P2 bug, label test-user) — `caco doctor schema reports
  drift on project_messages (3 EXTRA columns) and emits misleading
  'error:' prefix on stderr`. Test-user repro: `caco doctor schema;
  echo exit: $?` against cacophony 1.2.491 produced exit 2 with the
  whole report on stderr behind `error:`.

## Before state
- Repro on this host: `caco doctor schema` exited 2 with the entire
  table-by-table report on stderr prefixed by `error:` (because
  `dispatch_doctor_schema` returned `Err(CliError)` on drift, and
  the binary surfaces every CliError that way).
- The reported drift was real: `project_messages` had three EXTRA
  columns the binary baseline didn't list — `cc_target`,
  `delivered_at`, `read_at`. Cross-checked against
  `crates/caco-daemon/src/messaging.rs:384 (init_table)` — those
  columns are part of the canonical schema (`cc_target` per
  bd-? message-cc work, `delivered_at` per bd-91a14c, `read_at`
  per the original message-read tracking). The baseline in
  `crates/caco-cli/src/lib.rs:expected_schema_baseline` was lagging
  the daemon's actual `init_table`, so every clean install reported
  3 phantom drift columns.

## After state
- `expected_schema_baseline` now lists all 14 `project_messages`
  columns including `cc_target`, `delivered_at`, `read_at` with a
  bd-ded3df comment pointing at `messaging::MessageStore::init_table`
  as the source of truth.
- `dispatch_doctor_schema` now returns `Result<(String, bool),
  CliError>` instead of using `Err(CliError)` to signal drift.
  - The boolean second tuple element is `has_drift`.
  - Real failures (bad `--db` value, can't open DB, JSON serialise
    error) still come back as `Err(CliError)` and surface with the
    standard `error:` prefix on stderr.
- The `[doctor, schema]` arm of `dispatch()` now maps the tuple
  into an `Outcome` directly: `exit_code = 2` if `has_drift`, body
  on `stdout`, no `error:` prefix. `--json` mode is unchanged
  (always exit 0; tooling reads structure from the JSON body).
- End-to-end smoke after fix: `./target/debug/caco doctor schema`
  exits 0 with `0 drift column(s)`, `project_messages OK (14
  columns)`, empty stderr.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+106/-18):
  - `expected_schema_baseline`: added `cc_target`, `delivered_at`,
    `read_at` to the `project_messages` column list with bd-ded3df
    comment.
  - `dispatch_doctor_schema`: signature → `Result<(String, bool),
    CliError>`; both `Ok` exits return tuple; rewrote doc comment.
  - `dispatch()` `[doctor, schema]` arm: destructure tuple, map
    `has_drift` → `exit_code = 2`, body on stdout, no `error:`
    prefix. Removed the dead `Err` arm-translation now that real
    errors are still bubbled with `?`-equivalent semantics.
  - `doctor_schema_json_handles_missing_db_cleanly` /
    `doctor_schema_detects_missing_and_extra_columns`: updated
    callers to destructure the new tuple.
  - 2 new tests:
    - `doctor_schema_drift_returns_ok_with_has_drift_true` — text
      mode drift returns `Ok((report, true))`, body contains both
      EXTRA listing and summary footer.
    - `doctor_schema_arg_validation_still_returns_err` — bad `--db`
      still returns `Err`, preserving the `error:` prefix path
      for real failures.

## Operator-takeaway
- `caco doctor schema` against a healthy 1.2.499+ install will now
  exit 0 with all-OK output. If you see drift after upgrading,
  it's a *real* schema regression worth flagging.
- Shell scripts that gated on the previous behaviour
  (`caco doctor schema || alert`) keep working: drift still
  exits non-zero (now exit 2), only the output channel and prefix
  changed.
- For the canonical baseline going forward, edit
  `expected_schema_baseline` in lockstep with the relevant module's
  `init_table` (messaging, dynamic_registry, store) — the
  `expected_schema_baseline_matches_daemon_init_tables` test
  catches the baseline-lags-init direction; the new
  `cc_target/delivered_at/read_at` audit was forced by user
  reproduction, not by that test.

## Tests
- `cargo test -p caco-cli --lib doctor_schema` — 5/5 passed.
- `cargo test -p caco-cli --lib expected_schema_baseline` — 1/1
  passed.
- `cargo build -p caco --bin caco` — clean.
- `cargo clippy -p caco-cli --all-targets -- -D warnings` — clean.
- End-to-end: `./target/debug/caco doctor schema` exits 0, output
  on stdout, stderr empty.

# bd-b76723 — warn (or error under strict) on unrecognised CLI flags

## Goal
Stop every caco subcommand from silently accepting unknown flags so a
typo like `caco msg inbox --tial 5` no longer dumps the entire inbox
without complaint.

## Bead(s)
- bd-b76723 (P2 bug, label test-user) — `caco CLI: every subcommand
  silently accepts unknown flags`. Pairs with bd-c9d42c (unknown
  subcommands exit 0, already closed) and bd-8920ae (strict
  per-command validate_flags landed only in `caco ls`).

## Before state
- The dispatcher accepted any `--flag` token, stuffed it into the
  `parsed.flags` HashMap, and proceeded. Per-arm code only consulted
  the flags it cared about; everything else was silently ignored.
- Reproductions all returned normal output:
  `caco cert status --some-unknown-flag`, `caco config show --bogus`,
  `caco msg inbox --bogus --tail 1`, `caco bd list --bogus`.
- bd-8920ae had introduced strict `validate_flags` but only wired it
  into `caco ls`; a blanket strict rollout would false-positive
  because many dispatch arms read flags via ad-hoc `flags.get(...)`
  without keeping the static `ArgSpec` list authoritative.

## After state
- New `warn_or_error_unknown_flags(parsed_flags, allowed, command)`
  helper and `KNOWN_GLOBAL_FLAGS` constant covering the documented
  globals (`--json`, `--help`, `--config`, `--wait-daemon`).
- `dispatch()` calls it once at the top, before the giant match arm:
  ```rust
  if let Some(spec) = spec_for_path(path) {
      warn_or_error_unknown_flags(&parsed.flags, spec.args, &path.join(" "))?;
  }
  ```
- Default behaviour: a single warning to stderr per invocation,
  citing bd-b76723, listing the offending flag(s), and pointing at
  `CACO_STRICT_UNKNOWN_FLAGS=1` for opt-in escalation.
- `CACO_STRICT_UNKNOWN_FLAGS=1` (or `true`/`yes`/`on`) escalates to a
  `CliError` so CI and disciplined automation can refuse to run on
  typos.
- The existing strict bd-8920ae path in `caco ls` is unchanged.

## Diff summary
- `crates/caco-cli/src/lib.rs`:
  - New `KNOWN_GLOBAL_FLAGS` constant.
  - New `warn_or_error_unknown_flags` helper next to the existing
    `validate_flags` (preserved verbatim for `caco ls`).
  - In `dispatch`, invoke the helper between the function-prelude
    bindings and the existing `doctor_exit_override` initialisation.
  - 4 new tests:
    - `warn_or_error_unknown_flags_passes_when_all_known`
    - `warn_or_error_unknown_flags_warns_in_default_mode`
    - `warn_or_error_unknown_flags_errors_under_strict_env`
    - `warn_or_error_unknown_flags_allows_global_flags_unconditionally`

## Operator-takeaway
- Default is an advisory warning so nothing previously working
  starts failing on existing scripts; investigate the warning and
  either fix the typo or add the flag to the command's `ArgSpec`
  list.
- For CI / disciplined agent scripting set
  `CACO_STRICT_UNKNOWN_FLAGS=1` to make typos a hard error.
- A follow-up sweep should audit each dispatch arm's per-flag reads
  against its `ArgSpec` list so we can flip the default to strict.

## Tests
- `cargo test -p caco-cli --lib warn_or_error_unknown_flags` —
  4 passed.
- `cargo build -p caco-cli` — clean.
- Broader `cargo test -p caco-cli --lib` shows 957 passing / 13
  failing under shared run; spot-checked failures
  (`bd_update_rejects_no_field_flags`,
  `bd_dispatch_recovers_success_after_spawn_timeout`) all pass
  standalone — pre-existing env-leak cluster (separately tracked,
  not introduced by this change).

# Session summary — bd-fa4eb9: dedup --apply + caco-cli test fix-forward

## Goal

Land the bd-62a9d0 follow-up: `caco bd dedup --apply` closes
non-canonical members of each duplicate group as
`duplicate-of <canonical>`. Add `--interactive` prompt-per-group.
Also fix-forward the unrelated caco-cli lib-test compile breaks
on main so the new tests can run.

## Bead(s)

- `bd-fa4eb9` — `[bd-62a9d0 follow-up] caco bd dedup --apply`
- (drive-by) caco-cli broken-on-main fix-forward: `dispatch_agent_logs`
  4-arg test sites + duplicate `disable_hooks` profile field

## Before state

- `caco bd dedup` was read-only — slice 1 printed groups and a
  message saying `--apply` was deferred.
- `cargo check -p caco-cli --tests` failed: 6 errors from a
  `dispatch_agent_logs` signature change (`since: Option<&str>` added
  but 3 test sites still passed 4 args) and 1 duplicate
  `disable_hooks` field in a test profile literal.

## After state

- `caco bd dedup --apply` closes each group's non-canonical members
  via the existing close endpoint with
  `admin_override = true` and
  `admin_reason = "duplicate-of <canonical>"`.
- `--apply --interactive` prompts `[y/N/q]` on stderr/stdin per
  group; default (empty / N) skips, `q` aborts the rest.
  `--interactive` without `--apply` is rejected (nothing to confirm
  in dry-run); `--apply --interactive --json` is rejected (no TTY in
  JSON mode).
- Text output gains `[closed] / [skipped] / [error: ...]` suffixes
  on non-canonical rows and a `closed: N, skipped: M, failed: K`
  trailer.
- JSON output gains `apply / closed / skipped / failed /
  interactive_aborted` fields and per-member `outcome` strings.
- Exit code is 1 on any close failure.
- Validation extracted to `validate_dedup_apply_flags(apply,
  interactive, json)` so the rule can be locked with unit tests
  without spinning up a daemon.
- caco-cli lib tests compile and the 5 new dedup tests pass alongside
  the existing 890.

## Diff summary

- Commits: `cbe6d2cc`
- File: `crates/caco-cli/src/lib.rs` (+228 / -17)
- Tests: +5 (dry-run, apply alone, apply+interactive text,
  interactive-without-apply rejection, interactive-with-json
  rejection).
- Build + clippy clean on caco-cli.

## Operator-takeaway

Operators can now collapse the 988-draft pool dedup clusters
non-interactively (`caco bd dedup --apply`) or with confirm-per-group
(`--apply --interactive`). Each closure carries an audit-trail reason
(`duplicate-of bd-XXXX`) so future provenance queries can reconstruct
the dedup decisions. Sibling polish workers should mirror similar
audit-reason discipline on any future bulk-mutation CLIs.

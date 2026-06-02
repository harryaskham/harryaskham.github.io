# Session summary — destructive-idempotent CLI allowlist audit + bd-833a06/bd-8cf0ce reconciliation (bd-dd00c4)

## Goal

Extend the `is_destructive_idempotent_command` safety allowlist (introduced by
bd-833a06) so that destructive-but-MCP-retry-idempotent CLI commands refuse an
unrecognised safety flag instead of silently proceeding. Audit the
`idempotent: true` CLI spec universe for genuinely destructive leaves, add the
confirmed ones, and reconcile a contradictory test that bd-833a06 left behind.

## Bead(s)

- `bd-dd00c4` — Audit other destructive-but-idempotent CLI commands for the
  bd-833a06 unknown-flag footgun (extend `is_destructive_idempotent_command`
  allowlist). P3 bug, profile dev. labels: cli, destructive-guard, follow-up.
- Reconciled (folded in, no separate bead per coordination with aur-1): the
  broken-on-main test `checkout_regenerate_stays_idempotent_for_mcp_retry_bd_833a06`.
- Filed-or-pending (beads-write proxy flapping on this pre-fix node): a
  `[broken-on-main]` bead for an unrelated caco-daemon clippy `type_complexity`
  failure at `crates/caco-daemon/src/release_queue.rs:2009`.

## Before state

- Failing tests: `checkout_regenerate_stays_idempotent_for_mcp_retry_bd_833a06`
  FAILED on main (asserted `checkout regenerate` idempotent==true, but the spec
  is idempotent==false per bd-8cf0ce). Confirmed via queued
  `cargo test -p caco-cli --lib checkout_regenerate` (2 passed, 1 failed).
- `is_destructive_idempotent_command` allowlist contained only `checkout
  regenerate` (now redundant, since that command is already idempotent==false
  and covered by the bd-4c8fdd `force_strict = !idempotent` floor).
- Pre-existing, unrelated: `cargo clippy -p caco-cli --lib -- -D warnings`
  fails in the caco-daemon dependency (`type_complexity` in
  `detect_companion_releases`).

## After state

- Failing tests: none in the touched caco-cli destructive-guard area. Queued
  `cargo test -p caco-cli --lib` over checkout_regenerate / destructive_ /
  prune_commands / warn_or_error_unknown_flags → 10 passed, 0 failed; a focused
  re-run of the 5 destructive-guard tests → 5 passed.
- Allowlist now covers `checkout regenerate` (defense-in-depth), `agent prune`,
  and `prune run`. Audited-and-excluded: `bd snapshot rotate` and `codespace
  revoke/remove/rekey` (already idempotent==false), `theme create` / `mesh
  peers` (no CommandSpec in this CLI).
- The contradictory `stays_idempotent` test is removed (bd-8cf0ce is
  authoritative) with a reconciliation note; new tests pin both the idempotent
  preservation and the end-to-end allowlist-driven refusal.
- The unrelated caco-daemon clippy failure remains (separate broken-on-main
  bead); it is not introduced by this caco-cli-only change.

## Diff summary

- Code/content commit: `bd561efbd2` (final landed squash SHA from the
  reintegration receipt).
- Files touched: `crates/caco-cli/src/lib.rs` (+129 / -19): allowlist `matches!`
  arm extended with `agent prune` + `prune run`; helper + test doc comments
  updated; removed the failing `stays_idempotent` test; extended the
  `classified_` test; added `destructive_prune_commands_stay_idempotent_for_mcp_retry_bd_dd00c4`
  and `destructive_prune_commands_refuse_unknown_flag_via_allowlist_bd_dd00c4`.
- Tests: +2 added, 1 removed (failing/contradictory), 1 extended.
- Behavioural delta: `caco agent prune` and `caco prune run` now refuse an
  unrecognised safety flag (e.g. a mistyped `--dry-run`) before deleting
  on-disk state, instead of warning-and-proceeding. Their `idempotent: true`
  MCP-retry semantics are unchanged.

## Operator-takeaway

The destructive-command safety net now covers the two highest-risk
delete-on-disk-but-retry-idempotent CLI commands (`agent prune`, `prune run`),
closing the bd-833a06 footgun for them. The audit also surfaced and fixed a
broken-on-main test where bd-833a06 landed stale against bd-8cf0ce's later
`idempotent: false` decision for `checkout regenerate` — a reminder that the
allowlist (not the idempotent bit) is the right lever for destructive
strictness. A separate, unrelated caco-daemon clippy `type_complexity` failure
was found and routed as its own broken-on-main bead.

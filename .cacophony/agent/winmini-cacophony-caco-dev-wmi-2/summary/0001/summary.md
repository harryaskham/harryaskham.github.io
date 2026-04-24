# Session summary — bd-32a91d: caco update status accepts --stable-only

## Goal

Add the `--stable-only` flag to `caco update status` so the
read-only sister surface honours the same release-selection
filter as the destructive parent `caco update`.

## Bead(s)

- `bd-32a91d` — `caco update status missing --stable-only flag
  (works on parent 'caco update' but not on 'caco update status' —
  only the destructive parent supports the read-only filter)`.

## Before state

- `caco update --stable-only` worked (UPDATE_ARGS declared the
  flag, parent dispatcher threaded it into
  `UpdateCheckOptions { stable_only: true }`).
- `caco update status --stable-only` did NOT work — the `status`
  subcommand was constructed via `CommandSpec::leaf("status",
  ...)` with no ArgSpec and `dispatch_update_status` had no
  `stable_only` parameter, so the per-channel
  `check_for_update(...)` calls always used
  `UpdateCheckOptions::default()`.
- Operational signal in the bead: nightly channel last built
  3wk ago (2026-04-04), dev+hourly channels report 'no release
  found' — operators who wanted to filter `update status` to
  the stable channel had no read-only escape hatch.

## After state

- New `UPDATE_STATUS_ARGS` ArgSpec list declares `--stable-only`
  with the same wording as the parent.
- `update`'s `subcommands` entry for `status` is now a full
  `CommandSpec { args: UPDATE_STATUS_ARGS, ... }` rather than
  `CommandSpec::leaf(...)` so the flag is discoverable via
  `caco update status --help --json`.
- Dispatch arm in `run` parses `--stable-only` and forwards it
  to `dispatch_update_status(json_requested, stable_only, co)`.
- `dispatch_update_status` takes a new `stable_only: bool` and
  routes to `check_for_update_with_options(repo, ch, token,
  UpdateCheckOptions { stable_only })` for every channel thread.
- New test `update_status_subcommand_advertises_stable_only_flag`
  drives `caco update status --help --json` end-to-end and
  asserts `--stable-only` is in the args array — pins the help
  surface against future ArgSpec drift.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - Added `UPDATE_STATUS_ARGS` ArgSpec list.
  - Replaced `CommandSpec::leaf("status", ...)` with full
    `CommandSpec { ..., args: UPDATE_STATUS_ARGS, ... }`.
  - Dispatch arm threads `stable_only` into
    `dispatch_update_status`.
  - `dispatch_update_status` signature gains `stable_only: bool`;
    per-channel thread uses
    `check_for_update_with_options` instead of `check_for_update`.
  - 1 new test:
    `update_status_subcommand_advertises_stable_only_flag`.
- `cargo test -p caco-cli --lib
   update_status_subcommand_advertises_stable_only_flag`: pass.
- `cargo test-small`: 162 pass.
- `cargo clippy -p caco-cli --tests`: clean (16 pre-existing
  warnings unchanged from baseline).

## Operator-takeaway

The pattern from this bead is wider than `update status`: any
read-only `<verb> status` sister surface should mirror the
filters its destructive parent advertises. When you find a
`CommandSpec::leaf(...)` for a `status` subcommand, audit it
against the parent's ArgSpec for read-only-friendly flags
that should also apply to the read-only path.

The operational signal in the bead description (nightly +
dev/hourly channel CI gaps) is unrelated to this fix — it's a
separate CI-pipeline issue. The `--stable-only` filter on
`update status` lets operators sidestep noisy stale channels
while diagnosing whether stable has anything pending.

Out of scope here: the bd-b76723 unknown-flag warning class
(any documented affordance not declared in ArgSpec). That's
the same family as bd-53e157 (just-shipped --name on node
show); each command should fix its own ArgSpec rather than
suppress the warning globally.

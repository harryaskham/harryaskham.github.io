# Session summary — bd-53e157: --name alias declared + node status error wording aligned

## Goal

Fix two related papercuts on `caco node show` / `caco node status`
reported via the test-user pass on cacophony 1.2.519.

## Bead(s)

- `bd-53e157` — `caco node show: --name fires bd-b76723
  unrecognised-flag warning before fallthrough; node show vs node
  status inconsistent on --node bogus error path`.

## Before state

- `caco node show --name helsinki` produced
  `warning: bd-b76723: caco node show received unrecognised
  flag(s): --name. These were ignored by the dispatcher.`
  even though the dispatcher (bd-21d05e wiring at lib.rs ~11268)
  honours `--name` and fell through to produce the right output.
- `caco node show --node bogus` returned the gold-standard
  message:
  `node 'bogus' is not configured (no static or dynamic match;
  daemon is reachable — check spelling or run 'caco node list')`
  while `caco node status --node bogus` returned the bare
  `node 'bogus' is not configured`. Two sister surfaces, two
  drifted error formatters.

## After state

- `NODE_SHOW_ARGS` now declares `--name` as a documented alias
  for `--node`, suppressing the bd-b76723 warning on the
  documented affordance.
- `dispatch_node_status` adopts the same gold-standard wording
  as `dispatch_node_show`: probes /api/v1/health to distinguish
  daemon-alive (typo / unknown name → name the workaround
  `'caco node list'`) from daemon-down (dynamic nodes can't be
  resolved).
- Two new tests:
  - `node_show_args_declares_name_alias` — pins the ArgSpec
    declaration so a future ArgSpec refactor can't silently
    drop `--name` and re-introduce the warning.
  - `node_status_unknown_node_error_matches_node_show_wording`
    — source-greps both dispatch bodies for the gold-standard
    substrings so the two surfaces can't drift again.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `NODE_SHOW_ARGS`: register `--name` ArgSpec (~6031).
  - `dispatch_node_status`: replace bare error with the
    daemon-alive / daemon-down branched wording from
    `dispatch_node_show` (~59241).
  - Tests: 2 added.
- `cargo test -p caco-cli --lib node_show_args_declares_name_alias
   node_status_unknown_node_error_matches_node_show_wording`: 2/2 pass.
- `cargo test-small`: 162 pass.
- `cargo clippy -p caco-cli --tests`: clean (warnings unchanged
  from baseline).

## Operator-takeaway

The gold-standard error wording for `caco node show` /
`caco node status` is now the source-grep contract: any future
sister surface (e.g. `caco node disk` once bd-0502bc lands)
should adopt the same daemon-alive / daemon-down branched
wording so operators get consistent guidance regardless of
which subcommand they typed.

The bd-b76723 warning class is best handled by ensuring every
documented affordance (`--name`, `--id`, etc.) appears in the
relevant ArgSpec list — even when the dispatcher honours it via
fallthrough. The warning text "These were ignored by the
dispatcher" is factually wrong on fallthrough cases and only
serves to confuse operators who followed the help text.

Issue 3 of the bead description (suppress bd-b76723 on
successful fallthrough, or reword) is left unaddressed —
declaring `--name` in ArgSpec covers the immediate user-facing
case for `node show` but the wider warning rewording is a
cross-cutting change worth its own bead. Filing as follow-up
if not already covered by the bd-b76723 family.

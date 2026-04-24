# Session summary — bd-dfa77a Issue 1: caco launcher list --full flag

## Goal

Pin Issue 1 of the bd-dfa77a launcher sweep: text-mode output
truncates fingerprints to 12 chars + ellipsis (`4482d118a3ce…`)
which blocks `grep`-for-fingerprint workflows. Add `--full` flag
to unlock the complete 64-char fingerprint in text mode (json mode
already returns full fingerprints).

## Bead(s)

- `bd-dfa77a` — caco launcher list (P3 task, multi-issue). This
  session pins Issue 1 (`--full` flag, same truncation family as
  bd-0e1bac / bd-ad57d6 / bd-3bbc6f). The headline operational
  signal (v1.2.515-518 SKIPPED locally, single-binary-swap jump)
  is observation, not actionable in caco-cli — that's a deployment-
  process concern for the operator/cluster-ctrl. Issue 2 (`caco
  launcher` no-subcommand exits 0 with help instead of defaulting
  to `list`) is convention drift with mixed-fleet behaviour and
  not clearly the right fix without a dispatcher-wide convention
  decision. Issue 3 (envelope shape catalogue note) is a cross-
  cutting concern handled elsewhere (bd-5ae1ce family).

## Before state

```
$ caco launcher list
  archive: /home/harry/.cacophony/launcher/archive
  active:  4482d118a3ce…
  archived: 5 binaries

    1: v1.2.519 (4482d118a3ce…) archived 2026-04-23T09:23:07 (newest)
    ...
$ caco launcher list | grep 4482d118a3ce0b233cdd
[no match — full fingerprint unreachable from text mode]
```

## After state

```
$ caco launcher list --help
  --full   Show full 64-char fingerprints in text mode (default truncates to 12 chars + ellipsis).

$ caco launcher list --full
  archive: /home/harry/.cacophony/launcher/archive
  active:  4482d118a3ce0b233cdd925cf1c03ac4a8bc1cd6ef4c5283b73ced859046397b
  archived: 5 binaries

    1: v1.2.519 (4482d118a3ce0b233cdd925cf1c03ac4a8bc1cd6ef4c5283b73ced859046397b) archived 2026-04-23T09:23:07 (newest)
    ...
```

Default behaviour unchanged (still truncates to 12-char + ellipsis).
JSON mode unchanged (always returned full fingerprint).

## Diff summary

- 1 file changed, +30 / -8 (`crates/caco-cli/src/lib.rs`):
  - new `LAUNCHER_LIST_ARGS` declaring `--full`.
  - `LAUNCHER_SUBCOMMANDS.list` references it (was `args: &[]`).
  - `dispatch_launcher_list` gains `full: bool` parameter.
  - active fingerprint + per-archive fingerprint formatting
    branches on `full`.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco launcher list --full` unblocks fingerprint grep workflows
without changing the compact default. Same affordance pattern
that bd-0e1bac / bd-ad57d6 / bd-3bbc6f could adopt across the
truncation family (status launcher block, update status Nightly
column, cron list COMMAND column).

Push-discipline directive (helsinki ctrl + harry clarification):
- PROHIBITED: force push to main / beads / shared default branches.
- ALLOWED: force push to own agent branch (recovery path).
- REQUIRED: only `caco agent reintegrate` / `caco agent complete`
  move work onto main.
This session has only ever used local refs + daemon-mediated
reintegrate, so no audit needed.

# Session summary — bd-827c5e Issue 2: caco choices reissue --mode alias + docstring fix

## Goal

Pin Issue 2 of the bd-827c5e choices sweep: the `caco choices
reissue` help docstring used the bare word "mode" which led
operators to reach for `--mode` by muscle memory and hit the
bd-b76723 unrecognised-flag warning. Fix BOTH the alias gap (accept
`--mode` as an alias for `--notify-mode`) AND the docstring (name
the actual flag literally).

## Bead(s)

- `bd-827c5e` — caco choices sweep (P3 bug, multi-issue). This
  session pins Issue 2 (operator-affordance fix). Issue 1 is an
  operational/observability ask (`--max-age` / `--stale-after`
  filter on `choices current`, plus `caco doctor` surfacing stale
  choices, plus auto-resolution policy when `--notify-mode
  timeout-fallback` was supplied) — that's a multi-surface feature
  add belonging to choices subsystem ownership, not a quick fix.
  Bonus observation about msm-5's healthy use of choices for fake-
  bead handling is positive context, not actionable.

## Before state

```
$ caco choices reissue --help
Reissue an active choice with a new notify mode (escalate, broadcast).
...
$ caco choices reissue --choice-id zzz --mode escalate
warning: bd-b76723: caco choices reissue received unrecognised flag(s): --mode. ...
error: reissue failed: choice not found or already resolved (bd-14e75e)
```

## After state

```
$ caco choices reissue --help
Reissue an active choice with a new --notify-mode (escalate, broadcast). bd-827c5e: --mode accepted as alias.
...
$ caco choices reissue --choice-id <id> --mode escalate
[no warning; --mode resolved to --notify-mode; reissue proceeds]
```

`--notify-mode` still wins when both are supplied. Same family as
bd-3a6078 (`--id` accepted as alias for `--agent-id`).

## Diff summary

- 1 file changed, +18 / -3 (`crates/caco-cli/src/lib.rs`):
  - `CHOICES_REISSUE_ARGS` gains `--mode` ArgSpec (suppresses
    bd-b76723 unrecognised-flag warning).
  - `CHOICES_SUBCOMMANDS` reissue summary names `--notify-mode`
    literally and notes the alias.
  - Dispatcher resolves `--notify-mode` first, falls back to
    `--mode`.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco choices reissue --mode escalate` Just Works now (matches the
docstring's bare "mode" wording AND matches operator muscle memory
from `caco msg ... --mode`). Issue 1 (stale-choice filter +
auto-resolution + doctor surfacing) is a separate multi-surface
feature ask that remains in the bead body for the choices owner.

# Session summary — checkout regenerate refuses unknown flags before destructive wipe

## Goal

Close a CLI safety footgun discovered during live cs-2 canonical-checkout
recovery: `caco checkout regenerate` performs a destructive wipe + reclone of a
project's canonical checkout, but it silently *ignored* unsupported flags such
as `--dry-run` and ran the wipe anyway. An operator running a `--dry-run`
preview lost their checkout state. The goal was to make this destructive command
refuse unknown flags (joining the existing bd-4c8fdd strict-refusal cohort)
rather than warn-and-proceed.

## Bead(s)

- `bd-8cf0ce` — caco checkout regenerate silently ignores unknown flags before
  destructive wipe (should refuse like bd-4c8fdd)
- Carved out of `bd-7c065b` (P1 cs-2 node-infra checkout/SSH-identity repair),
  whose "Expected remediation" explicitly asked for this CLI safety slice. The
  node-infra repair itself remains separate and node-infra-owned.

## Before state

- Failing tests: none.
- `crates/caco-cli/src/lib.rs` `CHECKOUT_SUBCOMMANDS` `regenerate` `CommandSpec`
  was declared `idempotent: true`. The dispatcher derives
  `force_strict = !spec.idempotent`, so unknown flags only warned-then-proceeded
  (the read-only bd-b76723 path) even though regenerate wipes + reclones.
- Live impact: `caco @cs-2 checkout regenerate --project cacophony --dry-run`
  warned that `--dry-run` was unrecognised, then regenerated anyway.

## After state

- Failing tests: none.
- `regenerate` is now `idempotent: false`, so the dispatcher's force-strict path
  refuses unknown flags before the wipe. `regenerate` is `mcp_enabled: false`, so
  the `idempotent` field's bd-21c44d MCP retry-safety consumer does not apply;
  the only live effect of the bit here is unknown-flag strictness.
- `caco checkout regenerate --project X --dry-run` (or any unknown flag) now
  errors with the destructive-surface "Refusing to proceed" message and does NOT
  regenerate. A valid `--project`-only invocation still passes the flag gate.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Summary artefact commit: intentionally omitted (must not self-reference its
  own mutable SHA).
- Files touched: `crates/caco-cli/src/lib.rs`.
- Tests: +1 new focused test
  (`checkout_regenerate_refuses_unknown_dry_run_flag_bd_8cf0ce`) and `checkout
  regenerate` added to the existing
  `destructive_bd_subcommands_are_marked_non_idempotent` cohort assertion.
- Behavioural delta: unknown flags on the destructive `checkout regenerate`
  command are refused (non-zero exit, no wipe) instead of warned-and-ignored.

## Operator-takeaway

`caco checkout regenerate` is destructive (wipe + reclone) and now refuses any
unsupported flag before doing anything, so a stray `--dry-run` can no longer
silently destroy canonical-checkout state — the exact cs-2 incident that
prompted this. If a future change re-marks it `idempotent: true`, the cohort
test will fail and re-open the warn-then-wipe footgun.

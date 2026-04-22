# bd-ddb6ee — fix doctor's stale prune hint + hide already-pruned rows from `caco prune list`

## Goal
Make the disk-pressure recovery loop actually executable: the doctor
hint must point at a real command, and `caco prune list` must reflect
what's actually reclaimable.

## Bead(s)
- bd-ddb6ee (P2 bug, test-user). Two issues filed together:
  1. `caco doctor` recommended `caco prune --target` — no such flag.
  2. `caco prune list` reported 189 already-pruned rows totaling 0 B
     as if prunable, hiding the real reclaimable items.

## Before state
- Doctor's `agent_cargo_targets` hint (warn + error tier) emitted
  `Run \`caco prune --target\` across stopped agents …`.  The flag
  doesn't exist on either `caco prune` (just prints help) or
  `caco prune run` (silently swallowed under bd-b76723's warn-only
  mode).
- `dispatch_prune_list` enumerated every terminal-state agent
  including ones already pruned, with `RECLAIMABLE='0 B'` and
  `STATUS='pruned'`. With 189 such rows on the test host, the
  total reclaimable came out to 0 B — operator concluded "nothing
  to prune" while doctor was screaming about 102.5 GiB pressure.

## After state
- Doctor hint at warn and error tiers now reads:
  `Run \`caco prune run --state stopped --delete\` (or
  \`--state completed --delete\`) to drop completed/stopped agent
  checkouts; for in-flight workers, \`cargo clean\` inside the
  worker checkout is the only safe lever.` — both tiers tagged
  `(bd-ddb6ee)` for back-traceability.
- `dispatch_prune_list` now drops `agent.pruned` rows from the
  default listing.  New `--include-pruned` flag opts back in for
  forensics.  When the listing is empty *because* every candidate
  was already pruned, the empty-message names the hidden count and
  points at `--include-pruned`:
  ```
  caco prune list — no prunable agents (0 reclaimable;
   189 already-pruned hidden, pass --include-pruned to show)
  ```
- ArgSpec for `prune list` now lists `--include-pruned` so
  bd-b76723's unknown-flag detector doesn't warn on it.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+128/-3):
  - `expected_schema_baseline` neighbour: doctor `agent_cargo_targets`
    error+warning hints rewritten with the real prune commands.
  - `PRUNE_LIST_ARGS`: added `--include-pruned`.
  - `[doctor, schema]`-style `[prune, list]` arm of `dispatch()`:
    parse `--include-pruned` flag, thread into helper.
  - `dispatch_prune_list` signature gains `include_pruned: bool`.
    Filter chain drops `a.pruned` rows unless `include_pruned`.
    Empty-listing path counts already-pruned-in-scope and produces
    a more honest message.  `is_none_or` (clippy-preferred) used
    for the project filter.
  - 2 new tests in `tests`:
    - `prune_list_hides_already_pruned_by_default` — fixture is
      absent from default output; empty-message contains
      `already-pruned hidden` and `--include-pruned`.
    - `prune_list_include_pruned_shows_already_pruned_rows` —
      same fixture surfaces under `--include-pruned`.

## Operator-takeaway
- After binary roll, copy-pasting the doctor hint just works.
- `caco prune list` reflects true reclaimable count; pass
  `--include-pruned` for the legacy "show every terminal agent"
  view.
- Note this fixes the *reporting* and *hint*, not the underlying
  capacity: bd-fcc343 (shared-target-dir) is still the strategic
  fix for the 100+ GiB cargo-target footprint per host.

## Tests
- `cargo test -p caco-cli --lib prune_list` — 3/3 passed
  (`prune_list_help_renders` + 2 new bd-ddb6ee tests).
- `cargo build -p caco-cli` — clean.
- `cargo clippy -p caco-cli --all-targets -- -D warnings` — clean.

# Session summary — caco bd graph honesty fixes

## Goal

Close bd-e2f1dd — four sibling misses of the bd-bc52ef / bd-a08f85
silent-no-op-on-unknown-input sweeps, all in `caco bd graph`.

## Bead(s)

- `bd-e2f1dd` — caco bd graph drops the root bead when it has zero
  dependencies; --root <id> --include-closed returns '0 nodes' for valid
  IDs; --root <bogus> silently returns 0 nodes; --depth 0 / --status
  bogus also silent (CLI honesty sweep miss)

## Before state

```
$ caco bd graph --project cacophony --root bd-fa6f8b --format ascii --include-closed
# 0 node(s) shown                                  ✗ closed-root drop

$ caco bd graph --project cacophony --root bd-nosuchx --format ascii
# 0 node(s) shown                                  ✗ unknown id silent

$ caco bd graph --project cacophony --root bd-fa6f8b --depth 0 --include-closed
# 0 node(s) shown                                  ✗ depth 0 silent

$ caco bd graph --project cacophony --status bogus
# 0 node(s) shown                                  ✗ status enum silent
```

## After state

```
$ caco bd graph --project cacophony --root bd-fa6f8b --format ascii --include-closed
# 1 node(s) shown
(bd-fa6f8b closed) caco bd status per-project breakdown ...

$ caco bd graph --project cacophony --root bd-nosuchx --format ascii
error: unknown bead id: bd-nosuchx

$ caco bd graph --project cacophony --root bd-fa6f8b --depth 0 --include-closed
error: --depth must be >= 1 (use --depth 1 for the root + immediate
neighbours, or omit --depth for the full graph)

$ caco bd graph --project cacophony --status bogus
error: unknown --status value 'bogus'. Allowed: open, in_progress,
closed, deleted, draft, permanent, blocked
```

Happy path (open root with no deps + --include-closed) unchanged.

`cargo test-small` 57/57 PASS, `cargo clippy -p caco-cli --lib --tests`
clean. Two new unit tests
(`graph_validate_status_enum_rejects_typo`,
`graph_validate_depth_zero_rejects_with_friendly_msg`).

## Diff summary

- Commit: 4548b207
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +2
- Behavioural delta: bd graph rejects `--depth 0`, `--status` typos,
  `--root` typos with friendly errors; closed-root + `--include-closed`
  now renders the root; corpus-cap miss is patched by an explicit GET
  /beads/<id> when --root is missing from the listing.

## Operator-takeaway

The corpus fetch URL still uses `limit=2000` (and the cacophony corpus
is currently 2135). The fetch-cap issue is patched specifically for the
--root case (we GET the missing bead directly), but a broader graph
that happens to include neighbours older than the most-recent 2000
will still drop edges silently. File a follow-up if this becomes a
visible problem — the right fix is server-side pagination of the
listing, not client-side cap bumping (which loses correctness once the
corpus crosses any chosen ceiling).

The root-bypass-filters logic intentionally extends to `--status`
(not just `--include-closed`): if the operator explicitly --root'd to
a bead, they want to see it, full stop. A different read would be to
error "root excluded by --status filter" and let the operator drop the
filter — that's strictly more chatty and easier to add later if anyone
prefers it.

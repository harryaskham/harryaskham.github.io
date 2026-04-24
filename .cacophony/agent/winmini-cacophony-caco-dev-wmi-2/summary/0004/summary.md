# Session summary — bd-987435: caco foreach node --concurrency validates zero / bogus / over-ceiling

## Goal

Eliminate the three silent failure modes for `caco foreach node
--concurrency` so each gives clear, actionable feedback that
matches the gold-standard `caco bd stalled --limit` validator.

## Bead(s)

- `bd-987435` — `caco foreach node has THREE distinct
  --concurrency validator failures (zero/bogus/over-ceiling-999
  all silently accepted vs help saying 'ceiling: 8')`.

## Before state

- `caco foreach node --concurrency 0 status` → proceeded normally,
  silently treated as default.
- `caco foreach node --concurrency bogus status` → proceeded
  normally, silently treated as default.
- `caco foreach node --concurrency 999 status` → proceeded
  normally, silently used default (capped invisibly).

The dispatch arm was
`parsed.flags.get("--concurrency").and_then(|s| s.parse::<usize>().ok())`,
which throws away both the parse error and the over-ceiling
case, so `dispatch_foreach_node` never saw an invalid value.

## After state

The dispatch arm now does explicit validation before calling
`dispatch_foreach_node`:

- `--concurrency 0` →
  `error: --concurrency must be >= 1 (use --concurrency 1 for
  serial execution)`.
- `--concurrency bogus` →
  `error: invalid --concurrency value: 'bogus' (expected a
  positive integer between 1 and 8)`.
- `--concurrency 999` → ergonomic clamp-with-warning:
  `warning: --concurrency 999 exceeds the ceiling of 8;
  clamping to 8.` then proceeds with 8.

The over-ceiling path uses warn-and-clamp rather than hard-error
so the operator's intent is honoured and the warning text
explicitly names both the requested and clamped values
(stderr-only so stdout / `--json` consumers stay clean).

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `foreach node` dispatch arm: replaced
    `flags.get(...).and_then(parse::<usize>().ok())` with an
    explicit `match raw.parse::<usize>()` covering Ok(0),
    Ok(>FOREACH_MAX_CONCURRENCY), Ok(n), and Err.
  - 1 new test:
    `dispatch_foreach_node_validates_concurrency_flag` —
    source-greps the dispatch arm for the four validator
    substrings (`"--concurrency must be >= 1"`, the bogus-value
    template, `"exceeds the ceiling of"`, `"clamping to"`).
- `cargo test -p caco-cli --lib
   dispatch_foreach_node_validates_concurrency_flag`: pass.
- `cargo test-small`: 162 pass.

## Operator-takeaway

The pattern from this bead joins bd-2dc0c3 (caco ps) and
bd-bc3d7d (caco ls) as the third recent bounded-int /
enum-or-fk validator backfill. The `parse::<T>().ok()` idiom
on user-supplied flag values is a cluster-wide footgun: it
silently discards both the parse error and the out-of-range
case. Worth a project-wide grep for that idiom on any
operator-facing flag.

Out of scope here (kept explicit so a future claimant doesn't
think it landed):

1. **Issue 2 of the bead** — bogus inner subcommand
   (`caco foreach node bogussubcommand`) is currently dispatched
   per-node with each instance failing
   `error: no command supplied`. Distinguishing typo from
   missing-arg requires a foreach-side validation pass against
   the known command tree before fan-out, which is a deeper
   dispatch refactor.

2. **--node gap pattern** — the bead notes `caco status --node`
   doesn't work (5+ surfaces missing `--node`). That's the
   bd-5ae1ce family, not in this bead's scope.

Both deliberately deferred so the concurrency validator lands
cleanly.

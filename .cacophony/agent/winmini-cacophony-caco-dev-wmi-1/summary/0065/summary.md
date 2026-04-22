# Session summary — bd-ad57d6: update-status table truncation; agent-context help note

## Goal

Two CLI honesty papercuts:

1. `caco update status` table renderer used `{:<16}`
   without truncation; a 20-char `Latest` cell
   (`Nightly (2604040200)`) overflowed and glued
   into the `Published` cell with no separator.
2. `caco launcher --help` listed only `list` and the
   parent description promised "for rollback" — but
   `rollback` IS implemented; it's just hidden because
   `agent_safe = false` and test-user-hel runs with
   `CACOPHONY_AGENT` set.  Operators in agent context
   couldn't tell hidden subcommands existed.

## Bead(s)

- `bd-ad57d6` — P3 bug, test-user-hel filed.

## Before state

```
$ caco update status
Channel     Latest          Published            Update?
nightly     Nightly (2604040200)2026-04-04 04:55     yes
                              # ^^^ no separator

$ CACOPHONY_AGENT=1 caco launcher --help
Subcommands:
  list   List archived launcher binaries available for rollback.
  # ← rollback exists but hidden
```

## After state

```
$ caco update status
Channel     Latest          Published            Update?
nightly     Nightly (26040… 2026-04-04 04:55     yes
                          # ^ ellipsis + separator preserved

$ CACOPHONY_AGENT=1 caco launcher --help
Subcommands:
  list   ...
Usage: caco launcher ...
  (note: 1 additional subcommand(s) hidden in agent context;
   run from an operator shell to see them)
```

## Diff summary

- 1 file touched, +30 / −5:
  - `crates/caco-cli/src/lib.rs::dispatch_update_status`
    cells passed through `truncate_with_ellipsis` (the
    bd-c81d1a helper) at column-1 budgets (11/14/19);
    column separator preserved.
  - `crates/caco-cli/src/lib.rs::render_text_help`:
    in agent context, append a footer line counting
    hidden agent_safe=false subcommands.

## Verification

- `cargo build --bin caco`: clean.
- `caco update status` text branch shows ellipsis +
  separator for the long Nightly value.
- `CACOPHONY_AGENT=1 caco launcher --help` and
  `caco help launcher` both show the new footer.
- Operator shell (no `CACOPHONY_AGENT`) shows no
  footer (only displayed when filtering actually
  occurred).

## Operator-takeaway

Pattern: `truncate_with_ellipsis` (added for bd-c81d1a)
generalizes to any fixed-column table renderer; whenever
a `{:<N}` width spec is followed by another column,
truncate-with-ellipsis at N-1 keeps the separator
visible. Worth a follow-up audit of other table
renderers (`caco cron list`, `caco bd list`, etc.) using
similar pattern.

Pattern: agent-safety filtering is silent by default,
which is fine for MCP tool surfaces (the agent doesn't
need to know about the hidden surface area), but
**operators reading help text in agent context** lose
discoverability. The footer trades a single line of
noise for honest signal that there's more available
elsewhere.

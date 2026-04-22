# Session summary — bd-a29677: agent audit-reintegration --limit 0 errors

## Goal

`caco agent audit-reintegration --limit 0` silently
coerced 0→1 (`limit.unwrap_or(50).max(1)` in the
dispatcher). Test-user reported that --limit 0 and
--limit 1 returned different content (likely a real
race: agent-state in their environment changed between
the two invocations); on my repro both returned the
same single result, so no off-by-one bug — but the
silent coercion itself is the documented complaint:

> --limit 0 should refuse rather than silently return
> surprising different content from --limit 1.

`caco loop --count 0` already errors with
"--count must be >= 1"; matching that contract.

## Bead(s)

- `bd-a29677` — test-user filed. Closed.

## Before state

```
$ caco agent audit-reintegration --limit 0
Direct reintegration audit (last 7d)
  scanned candidates: 1
  ...                        # silently coerced to limit=1

$ caco loop --count 0 ...
error: --count must be >= 1   # documented contract
```

## After state

```
$ caco agent audit-reintegration --limit 0
error: --limit must be >= 1 (use --limit 1 to inspect a
single candidate, or omit --limit for the default of 50)

$ caco agent audit-reintegration --limit 1   (real result, unchanged)
```

## Diff summary

- 1 file touched, +14 / −5:
  - `crates/caco-cli/src/lib.rs`:
    - dispatch arm for `agent audit-reintegration`
      validates `--limit >= 1` after parsing; rejects
      0 with a hint pointing at `--limit 1` and the
      default of 50.
    - `dispatch_agent_audit_reintegration`'s `.max(1)`
      kept as defense-in-depth with a comment pointing
      at the dispatch-arm pre-validation.

## Verification

- `cargo build --bin caco`: clean.
- `--limit 0` → friendly error pointing at --limit 1
  and default.
- `--limit 1` → real result (unchanged).
- `--limit -1` → existing 'unsupported flag' error
  (parse arm catches the dash).

## Operator-takeaway

The `unwrap_or(N).max(1)` idiom is a silent-coercion
smell — it papers over operator typos that should be
loud. Where the value 0 has a meaningful interpretation
elsewhere in the CLI vocabulary (e.g. `--feed-tail 0`
disables the slice in fleet snapshot), keep the
coercion path. Where 0 is purely nonsensical, error.

`caco loop --count 0` is the existing reference
contract: ">= 1 required" with a hint pointing at the
default. Worth a follow-up sweep grep for `.max(1)` /
`.unwrap_or(1)` patterns on `--limit`-shape flags
across the dispatch surface; this is the 2nd such
papercut after a similar shape in the bd-c95d44 family.

Test-user's "different content" report didn't reproduce
on my run — likely a race condition in their
environment (running agents change state between two
back-to-back audits). The silent-coercion fix is still
correct and addresses the underlying smell.

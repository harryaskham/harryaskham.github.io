# Session summary — bd-8c5bda: agent merge-queue list --limit 0 errors (sibling of bd-a29677)

## Goal

Operator-takeaway sweep from bd-a29677. The `.max(1)`
silent-coercion smell appeared at 2 sites. bd-a29677
fixed audit-reintegration; this bead closes the same
shape on `agent merge-queue list`.

## Bead(s)

- `bd-8c5bda` — own follow-up. Closed.

## Before state

```
$ caco agent merge-queue list --limit 0
Merge queue (recent window: 24h)
  in-flight: 0       # silently coerced from 0 to 1
```

## After state

```
$ caco agent merge-queue list --limit 0
error: --limit must be >= 1 (use --limit 1 to inspect
a single entry, or omit --limit for the default of 50)

$ caco agent merge-queue list --limit 1
Merge queue (recent window: 24h)
  ...                                     (real result)
```

## Diff summary

- 1 file touched, +14 / −5:
  - `crates/caco-cli/src/lib.rs`:
    - dispatch arm for `agent merge-queue list`
      validates `--limit >= 1` after parsing.
    - `dispatch_agent_merge_queue_list`'s `.max(1)`
      kept as defense-in-depth.

## Verification

- `cargo build --bin caco`: clean.
- `--limit 0` → friendly error.
- `--limit 1` → real result (unchanged).

## Operator-takeaway

After this round: 0 remaining `.unwrap_or(N).max(1)`
patterns on --limit-shape flags in
`crates/caco-cli/src/lib.rs` (verified via grep).
The smell is closed across the dispatch surface.

If a future surface adds `--limit`, the convention is:
- Dispatch-arm pre-validation: parse, then explicit
  `n == 0` reject with a hint pointing at `--limit 1`
  and the documented default.
- `.max(1)` inside the dispatcher kept as
  defense-in-depth (with a comment pointing back at
  the dispatch-arm validation).

This is the 'don't silently coerce nonsense values'
counterpart to the silent-unknown-value family
(bd-126b99 ... bd-c95d44, 14 beads). Same overarching
principle: operator typos must be loud.

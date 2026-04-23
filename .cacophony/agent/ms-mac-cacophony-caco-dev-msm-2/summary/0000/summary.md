# Session summary — Reject anonymous-caller bead mutations (bd-43db78)

## Goal

Close the accountability hole that allowed 22 "unknown"-creator beads to be
filed against the cacophony project, including 3 spam beads at
2026-04-23T01:22:40Z that polluted the P0 queue and wasted msm-5 dev capacity.

## Bead(s)

- `bd-43db78` — Investigate: 3 fake beads (login/auth/JWT) filed at 01:22Z by creator='unknown'

## Before state

- Failing tests: 7 pre-existing broken-on-main (stack overflows in persistent_recreate family, filer.md YAML parse, retention_sweep)
- 22 historical bead_created events with `sender: "unknown"` in feed.jsonl
- `resolve_caller()` silently fell back to literal `"unknown"` when no identity was present
- No caller-identity validation at any bead-mutating endpoint

## After state

- Failing tests: same 7 pre-existing broken-on-main (unchanged by this diff)
- All bead-mutating endpoints reject anonymous callers with 400 + `missing_caller_identity`
- `bearer_auth_middleware` synthesizes `x-caco-caller` from token identity when absent
- Forwarded peer requests are exempt (originating node is enforcement point)
- 8 new passing tests covering the enforcement and regressions

## Diff summary

- Commits: 1 (pending)
- Files touched: `crates/caco-daemon/src/beads.rs`, `crates/caco-daemon/src/lib.rs`
- Tests: +8 new
- Behavioural delta: bead create/update/delete/claim/close/unclaim/expand/rewrite/dispatch endpoints now reject requests where the resolved caller is empty, whitespace-only, or the literal sentinel "unknown". Auth middleware synthesizes caller identity from bearer token claims when no explicit header is present.

## Operator-takeaway

The root cause of the fake-bead incident was the `beads/expand` endpoint called
from the cluster router without a `x-caco-caller` header. Any authenticated
request that omitted the header fell through to `"unknown"`. The two-layer fix
(middleware synthesis + handler rejection) ensures this cannot recur while
remaining transparent to all first-party callers. The 22 historical
"unknown"-creator beads are all pre-existing and benign (bd-sync close events
on legitimate beads from early project history).

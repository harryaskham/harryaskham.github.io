# Slice 12 — bd-896551: fix Session Summaries 500s

## Goal

Fix intermittent HTTP 500s on the caco-web Session Summaries view caused by the detail handler timing out after 30s.

## Bead(s)

- **bd-896551** (bug, P1) — caco-web Session Summaries view 500s and is unreadable.

## Root cause

`read_parsed_summary_from_state_branch` called `enumerate_summaries_from_state_branch` which performs a `git show` for **every** summary across all agents just to build the one requested record's metadata. For agents with 80+ summaries, that was ~90 git invocations per detail fetch. Under concurrent load, this exceeded axum's 30-second timeout and returned `{"code":"internal_error","message":"request timed out after 30s"}`.

## Before state

- Detail fetch: O(N) git operations where N = total summaries for the agent's project.
- Intermittent 500s confirmed: msm-5/86 failed on ~40% of rapid requests.
- Frontend showed bare "HTTP 500" error with no retry.

## After state

- Detail fetch: O(1) — exactly 3 git operations (`rev-parse` for tip, `git show` for body, `git ls-tree` scoped to the single summary directory for artefacts).
- Frontend retries once on 5xx with 1s backoff before showing error.

## Diff summary

```
 crates/caco-daemon/src/summary.rs      | ~50 lines changed
 crates/caco-web/static/summaries.js     |  6 lines added (retry)
```

## Operator-takeaway

Session Summaries should load reliably now. The detail handler went from scanning all summaries to reading just the one you clicked on.

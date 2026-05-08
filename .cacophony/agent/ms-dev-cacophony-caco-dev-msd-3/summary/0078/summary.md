# Session summary — summaries endpoint transport diagnostics

## Goal

Make `caco summaries list/show` stop misclassifying a summaries-only failure as a whole local-daemon outage. The operator-facing goal was to keep endless-profile memory rehydration diagnostics actionable: if the daemon is healthy but `/api/v1/summaries` fails, the CLI should say the summaries endpoint is unavailable rather than suggesting `caco up`.

## Bead(s)

- `bd-af4c60` — caco summaries list returns local transport_error while daemon doctor is healthy

## Before state

- Failing tests: no local test failure reproduced in this checkout; the bead reported `caco summaries list --agent ms-mac-cacophony-doctor --limit 3 --json` returning `ok:false` / `transport_error` while other daemon surfaces were healthy.
- Relevant metrics: a live `caco summaries list --agent ms-mac-cacophony-doctor --limit 3 --json` from this checkout succeeded in about 14 seconds, confirming the normal success path still works.
- Context: `caco summaries list/show` routed daemon request failures directly through the shared `bd_send_request` transport envelope, so a summaries-specific handler/read failure could present the same `failed to reach daemon — try: caco up` guidance as a real daemon outage.

## After state

- Failing tests: none in the focused queued validation.
- Relevant metrics: queued validation job `tj-f4520179` passed `cargo test -p caco-cli summaries_transport_error --lib && cargo check -p caco-cli`.
- Context: summaries list/detail requests now run a quick `/api/v1/node` health probe when their primary request yields a JSON `transport_error`. If that probe succeeds, JSON output is rewritten to a retryable `summaries_unavailable` diagnostic that keeps the original transport message as context.

## Diff summary

- Commits: `91a1098438`.
- Files touched: `crates/caco-cli/src/summary_cmd.rs`, `SPEC.md`, `README.md`, `AGENTS.md`.
- Tests: +2 focused caco-cli unit tests for rewriting summaries transport errors only when the daemon health probe succeeds.
- Behavioural delta: `caco summaries list/show --json` preserves ordinary transport errors when the daemon is actually unreachable, but reports a more specific summaries endpoint failure when the daemon remains reachable. Text mode similarly prefixes the error as a summaries endpoint outage when the daemon health probe succeeds.

## Operator-takeaway

The fix does not make slow/cold summary scans faster; it makes the failure mode truthful. If summaries fail while the daemon is otherwise healthy, agents and operators should now see a summaries-specific retryable diagnostic instead of being pointed at daemon restart recovery.

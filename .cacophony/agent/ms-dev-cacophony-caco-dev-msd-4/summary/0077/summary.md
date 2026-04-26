# Session summary — indeterminate bd claim payloads

## Goal

Make `caco bd claim` restart-window responses unambiguous in both text and JSON modes. The specific friction was that a daemon could return `ok=true` with an empty claim payload during a restart/lock-contention window, leaving agents unsure whether ownership had actually changed.

## Bead(s)

- `bd-44b293` — Make caco bd claim restart-window responses unambiguous

## Before state

- Failing tests: no standing failure, but existing text-mode coverage only guarded against `claimed: ?` output.
- Relevant metrics: text mode already refused placeholder success for empty `data`, but `--json` mode routed the daemon's `ok=true` envelope through unchanged, so a machine caller could still mistake an empty payload for a successful durable claim.
- Context: claim ownership verification is mandatory for workers, so indeterminate claim responses must be explicit and non-zero instead of requiring ad hoc list/show reconciliation.

## After state

- Failing tests: none in validation.
- Relevant metrics: `caco bd claim --json` now converts `ok=true` with missing `data.id`, `data.title`, or `data.assignee` into a structured `ok=false` / `indeterminate_claim` response with confirm-state guidance.
- Context: the text-mode guidance now also states that no durable ownership change is confirmed, while successful claim output remains unchanged when the daemon returns a complete payload.

## Diff summary

- Commits: `c30afb7ab`
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`
- Tests: +1 JSON-mode regression for empty claim payloads; existing text-mode empty-payload regression rerun; `cargo check -p caco-cli`; `cargo test-small`; `cargo fmt --all -- --check`.
- Behavioural delta: restart-window/empty-payload claim responses are treated as indeterminate failures across output modes rather than partial successes.

## Operator-takeaway

Agents now get a definitive non-zero, structured answer when `bd claim` cannot prove durable ownership, reducing racey “did I own it?” hand checks during daemon restart windows.

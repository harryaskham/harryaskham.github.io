# Session summary — create-and-claim indeterminate payloads

## Goal

Close the remaining ownership-reporting gap where `caco bd create --claim true` could report a successful created-and-claimed bead even when the follow-up claim response did not prove durable ownership. The operator-facing goal was to preserve the created bead id but force the caller to verify ownership before editing.

## Bead(s)

- `bd-855164` — caco bd create --claim can report success before ownership is durable

## Before state

- Failing tests: none standing, but the reported workflow showed `created and claimed` followed by `bd show` still reporting `open` and unassigned.
- Relevant metrics: the create path already returned a structured partial failure when the claim follow-up returned `ok=false`, and `bd claim` itself now rejected empty claim payloads. However, `create --claim --json` could still pass through an `ok=true` empty claim payload before the formatter validated canonical claim fields.
- Context: agents rely on create-and-claim output for ownership decisions, so missing `data.id`, `data.title`, or `data.assignee` must be treated as indeterminate instead of success.

## After state

- Failing tests: none in validation.
- Relevant metrics: `create --claim` now converts `ok=true` with missing claim payload fields into non-zero `indeterminate_claim` in JSON mode and a text error in text mode; the error preserves the created bead id in the confirmation guidance.
- Context: the behavior shares the `bd claim` indeterminate helper so claim and create-and-claim speak the same restart-window/ownership language.

## Diff summary

- Commits: `f034e07b9`
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`
- Tests: +1 JSON-mode regression for `bd create --claim` empty claim payload; existing partial-claim-failure and `bd claim` indeterminate tests rerun; `cargo check -p caco-cli`; `cargo test-small`; `cargo fmt --all -- --check`.
- Behavioural delta: a created bead with an unproven follow-up claim no longer reports claimed ownership; callers get explicit verification guidance instead.

## Operator-takeaway

Create-and-claim now follows the same publish-or-refuse posture as direct claim: if Cacophony cannot prove ownership, it says so clearly and keeps the created bead id available for a safe manual retry.

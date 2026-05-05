# Session summary — report indeterminate bead-create persistence after transport loss

## Goal

Close the control-plane gap where `caco bd create --claim true` can persist a bead, lose the daemon reply during a restart-window or transport failure, and then mislead the caller with a generic reachability error instead of a bead-aware recovery result.

## Bead(s)

- `bd-ae014b` — Report indeterminate bd create success when daemon reply fails after persistence

## Before state

- Failing tests: none specific to this bead, but no regression covered the create-path transport-loss case.
- Relevant metrics: `dispatch_bd_create(...)` already handled `claim_after_create_failed`, `claim_after_create_unverified`, and `indeterminate_claim`, but not a lost initial create reply after persistence.
- Context: a prior session observed `caco bd create --claim true` return `failed to reach daemon`, then a retry hit duplicate detection and `caco bd show` revealed the bead had already been created open and unclaimed.

## After state

- Failing tests: targeted new regression passes.
- Relevant metrics: `dispatch_bd_create(...)` now inspects transport-like create failures for an exact persisted match and returns `indeterminate_create` with the recovered bead id instead of a generic daemon-unreachable error when recovery succeeds.
- Context: the CLI now issues a bounded non-mutating follow-up list/read for exact duplicate criteria (title/description/type/priority, plus creator/status filters) and tells the operator exactly how to inspect and claim the persisted bead safely.

## Diff summary

- Commits: `86a497578`, `b732d5351`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +1 / -0 / flipped 0
- Behavioural delta: `caco bd create` now recovers a persisted bead after a transport-lost create response and surfaces a bead-aware `indeterminate_create` result with explicit next steps, instead of only reporting `failed to reach daemon`.

## Operator-takeaway

A create request that persists server-side but loses its reply no longer has to strand operators in ambiguous duplicate/retry territory: the CLI can now recognize the persisted bead and hand back the bead id plus safe follow-up guidance, which is especially useful during restart windows and transient proxy failures.

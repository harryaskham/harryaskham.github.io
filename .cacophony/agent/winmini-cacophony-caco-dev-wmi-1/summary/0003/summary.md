# Session summary — bd-24b557 ready list vs beadless claim alignment

## Goal

Fix the queue-contract mismatch where user-facing `caco bd list --ready` and ready counters showed permanent trackers or `[operator-action]` beads as if they were claimable work, even though beadless `caco bd claim` would correctly refuse to assign them. The goal was to make the visible ready queue match what an idle worker can actually pick up.

## Bead(s)

- `bd-24b557` — caco bd list --ready includes permanent and operator-action beads that beadless claim will skip
- discovered during queue-drain follow-up after `bd-8573ef`

## Before state

- Failing tests: no targeted unit test covered the exact ready-list/read-surface mismatch, but queue-drain triage on healthy nodes reproduced it consistently.
- Relevant metrics: `crates/caco-beads/src/store.rs::list_ready()` returned open plus permanent unassigned/unblocked beads, while `claim_next_ready()` separately skipped permanent-ish trackers, EPIC umbrellas, and operator-action beads. `crates/caco-daemon/src/beads.rs` and `crates/caco-daemon/src/modes.rs` used `list_ready()` directly for `?ready=true` reads and `ready_beads` / ready-count summaries.
- Context: the practical symptom was a drained implementation queue still looking non-empty because `caco bd list --ready` surfaced permanent workspace/STT umbrellas and `[operator-action]` work, while `caco bd claim` returned `No ready beads available to claim`.

## After state

- Failing tests: none in the focused beads/daemon lane.
- Relevant metrics: a shared `Bead::is_generic_ready_queue_candidate()` predicate now defines the generic beadless-ready queue contract. It excludes permanent trackers, EPIC umbrellas, and operator-action beads. `claim_next_ready()` now uses that shared predicate, and daemon read surfaces/counters apply the same filter when rendering `?ready=true` and ready counts.
- Context: explicit `--bead-id` claim remains broader for controller/operator workflows, but user-facing ready listings and counters now reflect only work that generic idle-worker claim can actually assign.

## Diff summary

- Commits: `271d15547`
- Files touched: `crates/caco-beads/src/model.rs`, `crates/caco-beads/src/store.rs`, `crates/caco-daemon/src/beads.rs`, `crates/caco-daemon/src/modes.rs`
- Tests: `cargo test -p caco-beads generic_ready_queue_candidate_excludes_permanent_epic_and_operator_action_bd_24b557 -- --nocapture`; `cargo test -p caco-beads claim_next_ready_skips_permanent_beads -- --nocapture`; `cargo test -p caco-beads claim_next_ready_skips_operator_action_beads -- --nocapture`; `cargo build -p caco-daemon`; `cargo build -p caco`
- Behavioural delta: `caco bd list --ready`, aggregate ready listings, project ready summaries, and daemon mode `ready_beads` counters now agree with beadless `caco bd claim` about what counts as generic ready work.

## Operator-takeaway

This closes a subtle but important queue-trust gap: the board no longer advertises permanent/operator-only work as generic ready inventory. When the ready queue is drained, the read surfaces now say so instead of sending workers chasing non-claimable beads.

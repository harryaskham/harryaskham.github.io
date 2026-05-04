# Session summary — shared-host validation wording now treats Cargo smoke checks as queued by default

## Goal

Take the next unresolved profile-audit bead after the persistent close-contract cleanup and resolve the prompt contradiction around validation on shared Cacophony hosts. The target was `bd-c040e9`, which captured the gap between generic direct-foreground validation wording and the actual shared-host queue policy used by this repository.

## Bead(s)

- `bd-c040e9` — [profile-audit] validation guidance contradicts project queue policy

## Before state

- The repo already had shared-host queue policy in `AGENTS.md` and role-specific queue-first wording in some specialist profiles.
- But the composed prompt surface still left room to read Cargo smoke checks as normal direct foreground validation, especially from:
  - generic worker/dev validation text
  - `caco-aks.md` saying “validate with targeted foreground commands” without distinguishing shell/chart checks from compiler-backed Cargo checks on shared hosts
- The operator policy relay during this session reinforced the same real-world ambiguity: raw Cargo on `ms-mac` was overloading the shared machine.

## After state

- Updated `AGENTS.md` to say explicitly that on shared `cacophony` hosts, compiler-backed Rust/Cargo smoke commands such as:
  - `cargo test-small`
  - `cargo check`
  - `cargo build`
  - `cargo clippy`
  are queue-worthy by default, with only source-only/lightweight checks kept local unless a role-specific instruction narrows the exception.
- Updated `.cacophony/profiles/dev.md` with the same explicit rule.
- Updated `.cacophony/profiles/worker.md` with the same explicit rule.
- Tightened `.cacophony/profiles/caco-aks.md` so its validation section now distinguishes:
  - local/source-oriented AKS shell/chart/operator-surface checks
  - queued Rust/Cargo validation from the checkout on shared hosts
- Extended the existing `caco-aks` profile test to lock in that queue-oriented wording.

## Diff summary

- Files touched:
  - `AGENTS.md`
  - `.cacophony/profiles/dev.md`
  - `.cacophony/profiles/worker.md`
  - `.cacophony/profiles/caco-aks.md`
  - `crates/caco-profile/tests/profile.rs`
- Validation:
  - `cargo test -p caco-profile caco_aks_profile_loads_as_persistent_direct_loop_with_pr_modes_available -- --nocapture`
  - `cargo build -p caco-config`
- Behavioural delta:
  - shared-host prompt text now treats Cargo smoke/build/test commands as queued by default instead of leaving them ambiguous as ordinary direct foreground validation
  - `caco-aks` now explicitly keeps only source-oriented AKS checks local and routes Rust/Cargo validation through `caco test run` / `caco build run`

## Operator-takeaway

This closes the prompt gap that led directly to the operator’s “do not run raw cargo on ms-mac” relay. The repo now states the intended contract much more plainly: on shared `cacophony` hosts, compiler-backed Cargo validation is queue-first by default, while local direct validation is reserved for genuinely source-only/lightweight checks unless a role explicitly narrows the exception.

# Session summary — persistent reintegrate flows now state their own bead-close contract

## Goal

Take the next unresolved profile-audit bead after the caco-aks specialist-stack cleanup and fix the conflicting bead-close guidance for persistent reintegrating workers. The issue was that generic one-shot wording about `caco agent complete` handling closure could still read as if it applied to persistent/endless reintegrate flows, even though worker/dev guidance and specialist profiles expected explicit post-landing `caco bd close`.

## Bead(s)

- `bd-a0158b` — [profile-audit] bead close guidance conflicts for endless reintegrating workers
- related previously landed context:
  - `bd-1cd45f` — endless generic claim instruction conflicts with caco-aks no-autoclaim
  - `bd-0ee3c9` — pi-self-ops bead-claim command conflicts with caco-aks lifecycle

## Before state

- `dev.md` already documented that reintegrated worker beads must be closed after successful landing on `main`.
- `caco-aks.md` already told the specialist to verify `origin/main` and then close the bead.
- But the more generic persistent/endless prompt layers did not restate that contract clearly enough, so older one-shot wording about closure being handled by `caco agent complete` could still be misread as applying to reintegrate-based workers.
- That ambiguity was especially confusing in reified prompt bundles for persistent specialist roles.

## After state

- Updated `.cacophony/profiles/endless.md` with an explicit section:
  - `## Bead close contract for reintegrating persistent workers`
- Updated `.cacophony/profiles/persistent-specialist.md` with a matching explicit section:
  - `## Bead close contract for specialist persistent workers`
- Both layers now say clearly that:
  - the generic `caco agent complete` closeout wording is for one-shot completion flows
  - persistent reintegrating workers must reintegrate, verify landing on `origin/main` (or configured target), and then perform the host role’s validated `caco bd close` step when required
  - workers must never close before landing and must not assume reintegration implicitly closed the bead unless the role says so
- Added targeted profile tests in `crates/caco-profile/tests/profile.rs` to lock in that close-contract wording.

## Diff summary

- Files touched:
  - `.cacophony/profiles/endless.md`
  - `.cacophony/profiles/persistent-specialist.md`
  - `crates/caco-profile/tests/profile.rs`
- Validation:
  - `cargo test -p caco-profile endless_profile_prompt_defines_post_landing_bead_close_contract -- --nocapture`
  - `cargo test -p caco-profile persistent_specialist_profile_disables_generic_pool_helpers -- --nocapture`
  - `cargo build -p caco-config`
- Behavioural delta:
  - persistent/endless reintegrate flows now explicitly define bead closeout instead of relying on readers to infer that the one-shot `complete` contract no longer applies
  - specialist persistent roles inherit the same clarification even when they do not compose the generic endless prompt body

## Operator-takeaway

This fixes a real prompt-contract hole rather than a single-profile paper cut. The repo now states one clear rule: `complete` auto-close wording is for one-shot completion flows, while persistent reintegrating workers must verify the landing and then run the role’s validated post-landing bead close step when required.

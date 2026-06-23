# bd-a4536d — caco-macos profile self-improvement (ms-mac Rust/nix + durability-verify + source-dedup)

## Goal
Per Harry's "use heavy context to make profile fixes" directive: capture durable
2026-06-22-session lessons into the shared caco-macos profile before /compact.

## Diff summary
- .cacophony/profiles/caco-macos.md (+63 lines, markdown-only):
  - Validation: ms-mac Rust/nix traps (eval-cache dead-substituter -> cache.nixos.org override; no real cargo gate -> self-validate queued cargo check on a devbox; avoid queued cargo on ms-mac).
  - Reintegration: verify lands on TRUE GitHub via gh-api compare + bd-82b6c2 silent-revert re-verify + failed-call-inconclusive guard.
  - Idle loop: source-dedup before building on the shared macOS surface + the decompose-route-verify-handoff triage pattern.
- Validated: profile-lifecycle-audit clean (83 profiles, no conflicts); git diff --check clean; frontmatter intact.
- Final landed squash SHA: see the reintegration receipt.

## Operator-takeaway
Shared caco-macos profile (caco-macos/0/1/2) now encodes the night's hardest-won
operational lessons so siblings don't re-learn them: the ms-mac eval-cache override,
the no-real-cargo-gate self-validation requirement, the true-GitHub durability verify,
and source-dedup on the fast shared macOS surface.

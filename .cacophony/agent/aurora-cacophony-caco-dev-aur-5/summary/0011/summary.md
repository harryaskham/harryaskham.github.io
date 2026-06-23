# aur-5 reintegration summary — bd-733f1e

## Bead
bd-733f1e (P3 task): caco profile compose --dry-run provenance omits reintegration fields (allowed_modes/mode/backend/push_tags).

## Problem
`caco profile compose --dry-run` provenance only surfaced 3 fields (permission_mode, memory, mcp_servers). It did NOT surface the composed reintegration policy fields. The composed-prompt JSON only echoes each profile's OWN frontmatter allowed_modes, not the resolved last-wins composition. This made the dry-run an incomplete/misleading verify surface — discovered during the cacophony-dev pr_auto_merge flip safety-gate verify (bd-560cad intersection->last-wins allowed_modes), where the push_tags-profile [direct]-only safety check could NOT be done via the dry-run and had to be verified by hand-reading profile frontmatter.

## Change
`crates/caco-profile/src/compose.rs`:
- New `profile_reintegration_provenance_rows(profiles)` builds deterministic provenance rows for the composed reintegration fields, mirroring `compose_reintegration`'s merge rules: `mode` must-agree, `allowed_modes` last-profile-wins (SPEC 16.5.1 / bd-560cad), `backend` last-wins (bd-259349), `push_tags` any-layer-true (bd-0044f4).
- Wired into `profile_basic_provenance_rows` (rows.extend), so the existing CLI dry-run renderer (caco-cli lib.rs:47480 calls profile_basic_provenance_rows) auto-surfaces the new rows. No CLI change needed.
- 2 unit tests: allowed_modes+backend last-wins (the dev pr_auto_merge case), and push_tags any-true + release stays [direct]-only (the safety case).

## Validation
Queued `cargo test -p caco-profile reintegration_provenance` (from agent checkout): both tests pass (2 passed; 0 failed). rustfmt-clean. The cacophony-fast-tests reintegration gate runs cargo check --workspace --tests + test-small + clippy on the merge commit.

## SPEC
SPEC 16.5.1 (deterministic composite profile composition / reintegration last-wins). Additive provenance surface improvement; no contract change.

## Diff
See the reintegration receipt for the landed squash SHA.

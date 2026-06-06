# Session summary — bd-87270a doctor benign-fsck partial-clone guidance

## Goal

Add a proactive `caco doctor` guard so blobless/promisor partial-clone agent checkouts do not get misdiagnosed as corrupt when manual `git fsck` prints many `broken link from tree ... to blob ...` lines for absent promisor blobs.

## Bead(s)

- `bd-87270a` — caco doctor: benign-fsck guard for blobless partial-clone agent checkouts (bd-590f73 fix #3 follow-up)

## Before state

- Core partial-clone failures from bd-590f73 were already fixed on main:
  - newly created agent checkouts propagate partial-clone config;
  - first-party rebase fetches through local canonical for partial/promisor cases.
- There was no first-party doctor row explaining that manual `git fsck` broken-link-to-blob output is expected for blobless/promisor checkouts.
- Agents/operators could interpret those fsck lines as object-store corruption and request unnecessary recreate/reset repair.

## After state

- `caco doctor` now scans persisted agent records under the local agents root.
- For each agent checkout, it detects partial/promisor status from:
  - the agent checkout itself (`extensions.partialClone`, `remote.origin.promisor`, or `remote.origin.partialclonefilter`), or
  - a local non-bare canonical `origin` that is partial/promisor (legacy worker case where child config was not propagated).
- Doctor emits an OK runtime row named `partial-clone agent checkout fsck guidance`.
- When partial/promisor checkouts are detected, the row explicitly says manual `git fsck` `broken link from tree ... to blob ...` output is expected/benign promisor-missing-blob behavior, not object-store corruption.
- For legacy workers backed by a partial canonical without child partial-clone config, the detail includes a soft migration hint to prefer agent recreate / checkout regeneration when convenient.
- README, SPEC, and AGENTS now direct agents/operators to trust the doctor guidance before declaring corruption.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-cli/src/lib.rs`
  - `SPEC.md`
  - `README.md`
  - `AGENTS.md`
- Behavioural delta:
  - added `PartialCloneAgentCheckoutNote`, `agent_checkout_partial_clone_note`, and `check_partial_clone_agent_fsck_guidance`.
  - wired the new doctor check into the runtime/storage area near agent directory diagnostics.
  - added a hint when partial-clone fsck guidance is relevant.
  - added tests for direct agent partial-clone detection and legacy local-canonical partial-clone detection.

## Validation

- `cargo test -p caco-cli doctor_partial_clone --lib` passed.
- `cargo check -p caco-cli --lib` passed.
- `cargo clippy -p caco-cli --lib -- -D warnings` passed.
- `./scripts/rustfmt-changed.sh crates/caco-cli/src/lib.rs` intentionally skipped the large crate root because its HEAD version is not rustfmt-clean; no unrelated formatting churn was introduced.

## Operator-takeaway

On blobless/promisor agent checkouts, large `git fsck` broken-link-to-blob output is now explicitly called out by `caco doctor` as benign unless other doctor/Git evidence indicates real corruption.

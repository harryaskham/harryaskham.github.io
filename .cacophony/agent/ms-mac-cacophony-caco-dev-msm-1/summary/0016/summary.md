# Session 0016 — bd-d21634: bead submission validation

## Goal
Establish guidelines + enforce a daemon-side validator that prevents
the class of vague / malformed beads bd-5993ed had to clean up.

## Decisions
- **Two-tier validator**: hard errors always block; soft warnings
  block unless `--force` is passed. Permanent / placeholder beads
  legitimately have minimal descriptions; `--force` is their escape hatch.
- **Internal callers bypass**: `BeadsStore::create_bead()` keeps its
  permissive shape so 250+ existing tests, sync reconcile, cross-
  project moves, and importers all continue to work without churn.
  Operator-driven submissions go through the daemon API which calls
  `create_bead_with_force()` instead.
- **Hard-error rules** focus on the bd-5993ed-class failures:
  `<UNKNOWN>` deps (LLM bead-expand sentinel), free-form English
  deps (`research-private-distribution`), uppercase / wrong-length
  bd-ids, empty title, oversized title.
- **Soft warnings** nudge toward better titles + descriptions.

## Code
- `crates/caco-beads/src/validation.rs` (NEW, 14k)
  - `Severity {Error, Warning}`, `ValidationIssue`, `ValidationReport`
  - `validate_create_params()` runs all rules
  - `is_valid_bead_id()` strict `bd-[0-9a-f]{6}` check
  - `strip_bracket_prefix()` so `[stt-xplat]` doesn't inflate length
  - 11 unit tests
- `crates/caco-beads/src/error.rs`
  - New `BeadsError::ValidationFailed { report, rendered }` variant
- `crates/caco-beads/src/store.rs`
  - `create_bead_with_force()` runs validator, calls inner
  - `create_bead()` now bypass-mode (internal callers unchanged)
  - `create_bead_inner()` private workhorse
- `crates/caco-daemon/src/beads.rs`
  - `CreateBeadRequest` gains `force: bool` (defaults false)
  - `handle_create_bead` calls `create_bead_with_force`
  - `bead_error_response_for` maps `ValidationFailed` to HTTP 422
    with structured `issues: [...]` array
- `crates/caco-cli/src/lib.rs`
  - `BD_CREATE_ARGS` gains `--force`
  - `dispatch_bd_create` forwards `body["force"] = true`
- `docs/bead-submission-guidelines.md` (NEW, 5.9k)
  - TL;DR, validator rules table, title style, description structure,
    deps rules, labels conventions, priority ladder, filing flow

## Tests
- `cargo test -p caco-beads --lib`: 253 passed (242 existing + 11 new)
- `cargo test -p caco-cli --lib bd_create`: 9/9 pass
- `cargo test-small`: 161 passed
- `cargo clippy`: clean (pre-existing warnings only)
- (Pre-existing stack overflow in
  `tests::config_distribute_with_distribute_command_node_uses_command`
  reproduces on stash-clean main; not introduced by this change.)

## Operator Acceptance Criteria
- [x] Guidelines document is created and shared
      (`docs/bead-submission-guidelines.md`)
- [x] Submission template/form is implemented
      (markdown body template embedded in guidelines doc; CLI flags
      surface the structured fields)
- [x] Validation rules are enforced
      (daemon-side validator wired into `handle_create_bead`)
- [-] Team is trained on proper bead submission
      (out of scope for code; doc is the artefact for fleet training)

## Mainline marker
`docs/bead-submission-guidelines.md` carries the bd-d21634 reference so
`caco bd close` mainline-validation passes.

## Constraints honored
- No docker
- Merge-queue mixin: caco-beads + caco-cli targeted + test-small + clippy
- Speaking claim/close via `caco msg speak`

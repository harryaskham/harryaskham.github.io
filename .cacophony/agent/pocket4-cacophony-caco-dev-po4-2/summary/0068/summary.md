# Session summary — bd-1d2935 part-3a: manual-edit land-gate predicate

## Goal
bd-1d2935 (P2, operator-requested): ergonomic `caco project reintegrate --checkout` / `caco config land`
path so a human can land a config-only edit made in a `caco project checkout` (which leaves the operator
on `manual/project_checkout/<project>`) without an agent cherry-pick proxy. This is the MAIN feature of
bd-e8dfe7 (part-1 actionable-message + part-2 dead-letter-suppression already landed); msm-3's Option A
design is preserved on the bead.

## Scope landed (part-3a — the safe, testable foundation; no publish-path behavior change)
The full feature is a long, publish-path-touching, operator-present effort (new CLI subcommand + daemon
land handler reusing the isolated-integration-checkout machinery). To land real, safe value without risking
the publish path (active fleet data-integrity concerns this session: bd-82b6c2 / bd-0ec380), this slice
lands the pure gate core msm-3 named as THE test target, in `crates/caco-daemon/src/reintegration.rs`:

- `is_manual_project_checkout_branch(branch)` and `is_agent_checkout_branch(branch)` — shared, `origin/`-tolerant
  branch classifiers (mirroring the close-validator `is_agent_branch_ref` notion in beads.rs).
- `ManualEditLandGate { Allow, Refuse(String) }` + `manual_edit_land_gate(head_branch, is_clean, reason)` —
  the pure publish-or-refuse precondition: allow iff (a) clean checkout, (b) HEAD is a NON-agent branch,
  (c) explicit audited `--reason`. Agent-branch is checked first so an operator mistakenly on an agent branch
  is routed to `caco agent reintegrate` rather than told their edit is uncommitted.
- Refactored `agent_branch_checkout_mismatch_message` to use `is_manual_project_checkout_branch`, removing the
  fragile inline `starts_with("manual/project_checkout/")` literal (dedup with the new gate).
- 5 unit tests (`*_bd_1d2935`): allow (manual + arbitrary non-agent + whitespace-trimmed), refuse agent-branch
  (incl. origin/ prefix + routing-precedence over dirty/no-reason), refuse dirty, refuse missing/empty reason,
  and the two classifiers.

`manual_edit_land_gate` / `ManualEditLandGate` are `pub` lib API (caco-daemon is a lib crate, so no dead_code);
they are the audited gate the future CLI subcommand + daemon land handler (part-3b) will share.

## Validation
Queued daemon test (shared-host policy): `cargo test -p caco-daemon --lib bd_1d2935` — daemon compiles clean,
all 5 tests pass (5 passed; 0 failed). The merge-queue reintegration gate (cacophony-fast-tests) re-runs
test-small + `cargo check --workspace --tests` + clippy on the merge commit.

## SPEC areas
Reintegration publish-or-refuse contract (SPEC 4930/4934 region; AGENTS.md reintegration policy). The gate
preserves "first-party publish-or-refuse, not manual git surgery."

## Remaining (part-3b — left OPEN, NOT closed; for a budget + operator-available heavy-Rust daemon worker)
- CLI: `caco project reintegrate --checkout <path> [--project <p>] --reason <r>` (or `caco config land`) under
  the caco-cli `project` command tree; mirror existing `--checkout`/`--project` flag plumbing.
- Daemon: a project-land handler that REUSES the direct-reintegration isolated-integration-checkout merge
  machinery (fetch+validate target, isolated integration checkout, squash-merge, verify reachable-from-remote,
  backup ref) but REPLACES `verify_agent_branch_checkout_preconditions` (reintegration.rs, keys on the AGENT
  branch) with `manual_edit_land_gate` (this slice). Keep the canonical-checkout preflight intact.
- SPEC/README/AGENTS docs for the new command. The bead notes it is "operator-present (touches publish path)"
  — part-3b ideally lands with operator availability.

## Diff
See the reintegration receipt for the landed squash SHA (this summary does not cite its own commit).

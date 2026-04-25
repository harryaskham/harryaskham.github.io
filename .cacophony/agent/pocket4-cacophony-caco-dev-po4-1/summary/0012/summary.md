# Session summary — bd-d5d9bd SPEC project-integration policy doc

## Goal

Document the existing `projects[].integration.default_intent` and
`projects[].integration.backend` policy fields in the normative
SPEC.md so the spec catches up with the implementation
(`ProjectIntegrationConfig` in `crates/caco-config/src/model.rs:2550`)
and the operator-facing docs (README, AGENTS, configuration.html)
that have already been updated.

## Bead(s)

- `bd-d5d9bd` — [docs] SPEC missing project integration policy fields (P3 bug)

## Before state

- `ProjectIntegrationConfig` exposed two policy fields landed under
  bd-2c6e5f:
  - `default_intent: ProjectIntegrationIntent` (direct / review /
    artifact_only / none)
  - `backend: ProjectIntegrationBackend` (local_merge / pull_request)
- `.cacophony/projects.yaml` already uses
  `default_intent: review` + `backend: pull_request`.
- README / AGENTS / docs/configuration.html had been updated to
  describe these fields.
- SPEC.md only described the older reintegration-mode text and the
  per-profile `reintegration.mode` knob; the project-level policy
  fields were undocumented in the normative spec, with the
  technical-writer profile prohibiting drive-by SPEC rewrites.

## After state

- Inserted a new normative section 17.1.2
  "Project-Level Integration Policy (bd-2c6e5f)" between 17.1.1
  (Structural Conflict Detection) and 17.2 (PR Modes).
- Both existing field semantics are spelled out with their value
  enums, their additive default-preserving rules, the resolution
  precedence vs per-spawn / per-profile mode selection, and the
  explicit non-goals (default_intent is policy intent and does not
  duplicate git topology; backend: pull_request reuses the
  project's gh wrapper rather than introducing a parallel GitHub
  config surface).
- Section numbers downstream of 17.1.1 are unchanged so existing
  cross-references and link anchors do not drift.

## Diff summary

- Commit: 0a9b22851
- Files touched: `SPEC.md` (+50 lines, one new subsection)
- Tests: no rust test pinned the new SPEC text. cargo test-small
  passes (262/262 caco-web lib tests).
- Behavioural delta: pure documentation; zero runtime changes.

## Operator-takeaway

Spec is now consistent with the implementation and with the
operator-facing docs that already reference these fields. The
section deliberately calls out two design invariants worth keeping
visible: (1) `default_intent` is a policy intent and not a
duplicated git-topology declaration — the target branches still
come from the existing remote/default_branch/reintegrate_target/
pr_base block; (2) `backend: pull_request` reuses the project's
existing `gh` wrapper and remotes, so operators are not forced to
maintain a parallel GitHub-only configuration tree to opt in. Both
were implementation choices that needed to be made normative before
a future "let's add a github: block" PR re-litigates them.

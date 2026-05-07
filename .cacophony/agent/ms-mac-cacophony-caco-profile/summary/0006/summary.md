# Session summary — controller bead-close guidance

## Goal

Resolve `bd-11e385`, a migrated profile-audit bead about controller prompts seeing apparently conflicting bead-close guidance: worker instructions warn not to close beads manually, while controller profiles list `caco bd close` as an available reconciliation surface. The goal was to make controller/admin close authority explicit without weakening one-shot worker lifecycle rules.

## Bead(s)

- `bd-11e385` — Profile audit: bead close authority conflicts for controllers

## Before state

- Failing tests: none known before editing.
- Relevant metrics: `bd-11e385` was open after migration from `collective/bd-65c7c6`; related profile-audit beads showed controller lifecycle wording had already been partially cleaned up.
- Context: `.cacophony/profiles/controller.md` and `.cacophony/profiles/project-controller.md` listed `caco bd close`, but did not say how that relates to generic worker-only “do not manually close” guidance.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `git diff --check` passes; `cargo test -p caco-profile scopes_bd_close` passes; `cargo test -p caco-profile controller_plugin_agent_matches_generated_runtime_agent` passes.
- Context: controller and project-controller profile bodies now state that `caco bd close` is a controller/admin reconciliation action after first-party mainline evidence or explicit audited admin close, and that generic worker no-close guidance applies to one-shot worker closeout rather than controller reconciliation.

## Diff summary

- Commits: `e44811a0a5` plus this summary commit.
- Files touched: `.cacophony/profiles/controller.md`, `.cacophony/profiles/project-controller.md`, `crates/caco-profile/tests/profile.rs`, `plugins/caco-agent/agents/controller.md`, and `plugins/caco-agent/agents/project-controller.md`.
- Tests: +2 profile assertions for controller/project-controller close guidance; generated controller plugin wrappers refreshed.
- Behavioural delta: reified controller prompts should now be clearer about when controller closeout is allowed, while worker-owned bead closeout remains lifecycle-managed.

## Operator-takeaway

Controller agents now get explicit, role-scoped bead-close guidance: they may use `caco bd close` for validated reconciliation/admin cases, but that does not license one-shot workers to bypass their normal completion lifecycle.

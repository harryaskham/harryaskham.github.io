# Session summary — project selection policy

## Goal

Define the global-versus-default behaviour for project-scoped commands so omitted `--project` is predictable and global aggregation remains explicit.

## Bead(s)

- `bd-09d9cf` — Design global results behavior for project-scoped commands

## Before state

- Failing tests: none.
- Relevant metrics: `bd-b43a92` had fixed `default_project` resolution, but the broader policy still needed to say whether omitted `--project` should mean default project or all projects.
- Context: `caco bd search --all-projects` already existed as an explicit all-project pattern, while runtime shorthand commands intentionally stayed stricter.

## After state

- Failing tests: none.
- Relevant metrics: SPEC, README, AGENTS, and a design note now state that project-scoped commands default to one resolved project; global results require an inherently global command or explicit opt-in such as `--all-projects`.
- Context: the policy also requires project identity in merged output and clear help semantics when a command grows all-project mode.

## Diff summary

- Commits: `29fb845f9`
- Files touched: `SPEC.md`, `README.md`, `AGENTS.md`, `docs/design/project-scoped-command-selection.md`
- Tests: `git diff --check`
- Behavioural delta: no runtime code changed; this lands the normative command-selection contract for future CLI work.

## Operator-takeaway

The decision is now explicit: omitting `--project` uses the resolved/default project, not every project. All-project output stays opt-in and labelled.

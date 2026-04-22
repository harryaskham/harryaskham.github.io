# Session summary — bd-0977ba agent_defaults.profile validation

## Goal

Surface typos in project `agent_defaults.profile` at config-load
time, not at first-spawn time. The persistent-agent path already
checks profile references against the discovered set (warning);
the project-defaults path did not. Close the gap with a hard
validation error that names the project, the missing profile, and
gives a one-line remediation hint.

## Bead(s)

- `bd-0977ba` — caco project create: no smoke-test that the
  project-defaults.yaml profile-stack actually resolves before
  persisting.

## Before state

A typo like `agent_defaults: { profile: typo-profile }` in a
project block validated cleanly via `caco config validate` (and
`caco config validate --strict`). The error only surfaced when an
agent in that project was actually spawned — minutes or hours
later, far from the operator's edit context. The persistent-agent
path had this check (as a warning, not error) but the project-
defaults path was uncovered.

## After state

`validate_config_with_extra_profiles` now rejects unknown
`agent_defaults.profile` references with:

```
project '<name>' agent_defaults.profile '<missing>' does not match
any configured profile (declare it under config.profiles, drop a
.md file under .cacophony/profiles/, or use one of the embedded
caco:* profiles)
```

Both `Single("name")` and `Composite([...])` selections are
checked. The optional `caco:` prefix is stripped before lookup so
embedded canonical profiles match the same way they do in the
persistent-agent path. Every missing entry in a composite is
reported in a single validation pass.

## Files touched

- `crates/caco-config/src/validate.rs` (+253 / -0).

## Diff summary

Single-file addition to `crates/caco-config/src/validate.rs`. Two
production-code blocks plus a fresh `#[cfg(test)]` submodule:

1. New pure helper `missing_profiles_in_selection(&ProfileSelection,
   &HashSet<&str>) -> Vec<String>` near
   `check_persistent_decl_missing_profiles`. Walks the selection,
   strips `caco:` prefix, returns names not present in the
   supplied set. Pure (no I/O), so unit-testable with synthetic
   sets.
2. New validation block inside `validate_config_with_extra_profiles`
   (placed immediately after the `agent_defaults.preset` check
   for visual locality). Loops over `config.projects`, reads
   `project.agent_defaults.profile`, runs the helper against the
   merged `profile_names` set (config.profiles + extra_profile_names
   already accumulated upstream), and pushes one hard error per
   missing entry into the validation batch.
3. New `#[cfg(test)] mod agent_defaults_profile_bd_0977ba` with
   11 tests: 5 unit tests for the helper (Single present/absent,
   `caco:` prefix strip, Composite missings collected, Composite
   fully present), 6 end-to-end tests through
   `validate_config_with_extra_profiles` (acceptance, error
   shape, multi-missing composite, `caco:` acceptance, no-profile
   passthrough, project name in error).

Severity choice — hard error vs warning: the persistent-agent
counterpart emits a warning because a missing-profile persistent
gets *skipped* at spawn (rest of system still works). But
`agent_defaults.profile` is the per-project default — every spawn
in that project picks it up. A typo silently breaks every new
agent. Bead text "rejects with actionable error" confirms the
intended severity. Used the validation-batch path so multiple
errors aggregate.

## Operator-takeaway

`caco config validate` (and `caco config validate --strict`) now
catches `agent_defaults.profile` typos before they propagate to
spawn. Operator workflow is unchanged — same command, same exit
codes, same JSON shape — but a previously-silent failure mode is
now eagerly surfaced. No migration needed; existing valid configs
continue to validate (the new check only fires when a profile
name fails the merged-set lookup, which previously meant a
guaranteed spawn-time failure anyway).

Out of scope for this slice (bead items 2-3): a stand-alone
`caco project lint` command, and equivalent eager checks for
`hook_mixins`, `mcp_servers`, `scopes`. Those pair with bd-aac755
and bd-8a56ce which are already in_progress.

## Validation

- `cargo test -p caco-config agent_defaults_profile_bd_0977ba`:
  11/11 PASS.
- `cargo test-small`: 204+109+731+291+18+2815+54 PASS green
  (caco-config test count delta +11, 720 → 731).
- `cargo clippy --workspace --all-targets -- -D warnings`: clean
  (1m20s).

## Notes / follow-ups

- A natural follow-up: extend the same eager-check pattern to
  `agent_defaults.preset` already exists; `hook_mixins` /
  `mcp_servers` / `scopes` pair with the in-flight beads. Worth
  a tracking bead for the union after the in-flight ones land,
  to ensure no silent-spawn-failure surface remains.
- `caco project lint` (bead item 2) is small once items 1 and 3
  are unified: just a thin CLI wrapper over the same validator
  with project-scope filtering. Defer until an operator workflow
  asks for the standalone command.

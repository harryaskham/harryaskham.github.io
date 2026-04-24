# Session summary — bd-909491: `--profile-path` for ad-hoc agent spawns

## Goal

Add `--profile-path <file.md>` to `caco agent new` and `caco bd dispatch`
so operators can test a profile draft directly from disk without first
installing it under `.cacophony/profiles/`.

## Bead(s)

- `bd-909491` — Add `--profile-path <file.md>` to `caco agent new/dispatch`
  for ad-hoc profile loading.

## Before state

- `caco agent new` only accepted `--profile <name>`.
- `caco bd dispatch` only accepted `--profile <name>`.
- Daemon profile resolution in `crates/caco-daemon/src/agent/profile.rs`
  only searched `profile_search_dirs()` / embedded assets by name.
- A draft profile file sitting somewhere on disk could be loaded by
  `caco_profile::load_profile(&path)`, but there was no first-class CLI
  path to get that file into a spawned worker.

## After state

- `caco agent new` now accepts `--profile-path <file.md>`.
- `caco bd dispatch` now accepts `--profile-path <file.md>`.
- `--profile` and `--profile-path` are mutually exclusive.
- The CLI canonicalizes `--profile-path` to an absolute `.md` file path
  before sending it.
- Daemon profile resolution now detects when the supplied profile string is
  actually an on-disk file path and loads it directly via
  `caco_profile::load_profile`, bypassing named profile discovery / embedded
  fallback.
- Direct-path profiles also participate in `composes:` expansion, because
  `expand_composes_visit()` now understands path-backed entries too.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
  - `crates/caco-daemon/src/agent/profile.rs`
  - `README.md`
  - `AGENTS.md`
  - `docs/agents.html`
- Behavioural delta:
  - Added `--profile-path` arg specs to `AGENT_NEW_ARGS` and
    `BD_DISPATCH_ARGS`.
  - Added `resolve_profile_arg()` helper in `caco-cli` to enforce
    `--profile` xor `--profile-path`, validate `.md`, and canonicalize
    the path.
  - `dispatch_agent_new()` now skips name-inventory preflight warnings for
    direct-path launches, because ad-hoc files are intentionally absent from
    profile inventory.
  - `dispatch_bd_dispatch()` now threads direct-path profile selection into
    the spawned agent request body.
  - `profile_direct_path()` in daemon profile resolution lets
    `expand_composes_visit()`, `resolve_profile_with_overrides()`, and
    `resolve_composite_profile_with_overrides()` load ad-hoc paths directly.

## Embedded artefacts

- none

## Operator-takeaway

This is a contained quality-of-life spawn feature, not a broad refactor.
You can now test a draft profile file directly with `caco agent new` or
`caco bd dispatch` before promoting it into `.cacophony/profiles/`, and the
normal profile bridge / `composes:` machinery still applies.

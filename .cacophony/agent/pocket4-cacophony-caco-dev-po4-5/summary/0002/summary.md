# Session summary — Remove deprecated `caco hello-world` command

## Goal

Close the bd-1401f4 cleanup by removing the deprecated
`caco hello-world` command and all of its supporting code, tests,
and documentation references. The command had been superseded by
`caco status` + `caco version` for first-run smoke-checking and was
listed only as a vestigial example in the help table.

## Bead(s)

- `bd-1401f4` — Remove old 'caco hello-world' command (P3, cleanup,
  cli, deprecation)

## Before state

- `caco hello-world [--json]` advertised in the top-level command
  table (`HELP_JSON_LISTS_REQUIRED_COMMAND_FAMILIES` test asserted
  it must be present).
- 1 dispatch arm + 1 dedicated render function
  (`render_bootstrap_payload`) + 5 supporting `Serialize` structs
  (`BootstrapPayload`, `RuntimeSnapshot`, `NodeSnapshot`,
  `ListenerSnapshot`, `ListenerPair`).
- README, GETTING-STARTED, AGENTS, and flake.nix smoke-check all
  referenced `caco hello-world --json`.
- `nix flake check` ran the hello-world smoke derivation.
- 1 dedicated integration test
  (`hello_world_json_matches_localhost_bootstrap_contract`) +
  1 multicall-alias test pinned to `caco_hello-world`.

## After state

- `caco hello-world` returns `error: no command supplied`.
- `caco --help --json` no longer lists `hello-world` in its
  subcommand array.
- `caco version --json` is the new smoke-check leaf used by
  `nix flake check`, README, and GETTING-STARTED.
- All 5 hello-world-only structs and `render_bootstrap_payload`
  removed.
- `resolve_static_local_api_port` kept with `#[allow(dead_code)]`
  because it still has unit-test coverage and is the canonical
  derivation of the local API port for any future bootstrap-style
  surface.

## Diff summary

- Commit: `2e87885da bd-1401f4: remove deprecated 'caco hello-world'
  command`
- Files touched: 6 (`crates/caco-cli/src/lib.rs`,
  `crates/caco/tests/cli.rs`, `flake.nix`, `README.md`,
  `GETTING-STARTED.md`, `AGENTS.md`)
- Net: +44 / -214 lines (mostly deletions; small docs + test
  edits).
- Tests: -1 dedicated integration test deleted
  (`hello_world_json_matches_localhost_bootstrap_contract`);
  multicall-alias test re-targeted to `caco_version` and still
  passes; `help_json_lists_required_command_families` updated to
  drop the `hello-world` expectation and still passes;
  `cargo test-small` green (162 passed, 0 failed).
- Behavioural delta: `caco hello-world` no longer exists; first-run
  smoke contract is now `caco version --json` (returns
  `{version, commit, commit_timestamp, ...}`).

## Operator-takeaway

`hello-world` is gone everywhere — code, tests, docs, flake smoke,
help-JSON contract. Anyone reading the GETTING-STARTED or README
will now be pointed at `caco version --json` for the trivial smoke
check. The dropped `BootstrapPayload`/`Runtime/Node/Listener*` struct
family was hello-world-only; if a future first-run dashboard wants a
similar shape it can rebuild it on top of the still-living
`resolve_static_local_api_port` + `resolve_static_cluster_contract`
helpers.

# Session summary — config-level daemon env vars (services.caco-daemon-extra-env)

## Goal

Give operators a portable, config-level way to set the daemon's process
environment without a Nix/Home-Manager rebuild and in non-Nix environments
(containers, AKS pods, Codespaces, manual installs). The motivating case is
setting `GIT_LFS_SKIP_SMUDGE=1` (and similar, e.g. `PULSE_SERVER`) so the
daemon and the child git/subprocess operations it spawns — notably the
reintegration integration-checkout clone — inherit it declaratively.

## Bead(s)

- `bd-3201f3` — Add config-level daemon env vars (`services.caco-daemon-extra-env`
  in config.yaml) so daemon environment can be set without a Nix rebuild and in
  non-Nix environments (P2 feature; operator request Harry via caco-ctrl).

## Before state

- No portable mechanism: daemon env could only be set via the Nix Home-Manager
  `services.cacophony.extraEnv` option (heavy: full Nix rebuild) or not at all
  in non-Nix environments.
- `docs/config-schema/services.html` had no `caco-daemon-extra-env` row; the
  top-level `services` schema section listed 91 rows.
- Failing tests: none.

## After state

- `services.caco-daemon-extra-env: { KEY: VALUE }` is parsed from config and
  applied via `std::env::set_var` at `caco daemon` bringup BEFORE the tokio
  runtime starts (pre-worker-threads, so set_var is thread-safe), so the daemon
  process and its child git/subprocess ops inherit the env. Explicit config
  values override inherited values (including the Nix `extraEnv`) for processes
  spawned after bringup.
- Generated config-schema docs now document the key: `services` section is 92
  rows; `docs/config-schema/services.html` + `index.html` regenerated and
  `caco-config-schema-docs-gen --check` is green.
- Failing tests: none. `cargo check --workspace --tests` + `cargo test-small` +
  `cargo clippy --workspace` run on the merge commit by the reintegration gate.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- `crates/caco-config/src/model.rs`: `Services.caco_daemon_extra_env:
  Option<BTreeMap<String,String>>` (serde `default` + rename
  `caco-daemon-extra-env` + skip_serializing_if) + `caco_daemon_extra_env_pairs()`
  resolver (BTreeMap-sorted) + unit test
  `caco_daemon_extra_env_pairs_resolves_bd_3201f3`; plus the top-level `services`
  SchemaSection row for `caco-daemon-extra-env` (this drives the config-schema
  docs).
- `crates/caco-cli/src/lib.rs`: capture env pairs before `config` moves into
  `DaemonConfig`, then `set_var` each before the runtime spawns.
- 32 `Services {…}` literal sites across caco-config + caco-daemon (src + tests)
  updated with `caco_daemon_extra_env: None,`.
- `docs/config-schema/services.html` + `docs/config-schema/index.html`:
  regenerated to include the new key (services 91→92 rows).
- Docs: `AGENTS.md`, `README.md`, `SPEC.md` — documented the portable
  config-level daemon env + precedence vs the Nix `extraEnv`.
- Tests: +1 unit test; schema-docs `--check` green.
- Behavioural delta: a new optional config key; unset preserves current
  behavior (no env applied).

## Operator-takeaway

Daemon process env (e.g. `GIT_LFS_SKIP_SMUDGE=1`, `PULSE_SERVER`) can now be set
declaratively via `services.caco-daemon-extra-env` with just a config change +
`caco restart` — no Nix rebuild — and it works in containers/AKS/Codespaces. It
complements (does not replace) the Nix `extraEnv`; explicit config values win for
processes spawned after bringup. The key is a kebab-case sibling of
`services.caco-daemon` because that key is a bare listener spec that cannot nest.
A correction over the first implementation pass: the top-level `services` schema
section IS surfaced in the generated config-schema docs, so the docs were
regenerated to include the new key (the earlier note that no regen was needed was
wrong). A possible follow-up is a node-scoped override
(`nodes[].services...`) if operators want per-node daemon env.

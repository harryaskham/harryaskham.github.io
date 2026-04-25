# Session summary — bd-523935 validate sibling-profiles discovery

## Goal

Make `caco config validate --config <checkout>/.cacophony/config.yaml`
discover profile `.md` files in the same checkout's
`.cacophony/profiles/` directory, so an operator authoring a new
profile + matching `agents.persistent.<a>.profile: <x>` declaration
in the same checkout no longer sees a confusing bd-7edbbd
"profile did not match any configured profile" warning that only
clears after the daemon canonical checkout syncs.

## Bead(s)

- `bd-523935` — Make local config validate discover same-checkout profile files (P3 task)

## Before state

- `discover_config_profile_names(&config)` scanned only:
  - configured projects' canonical checkout dirs (under runtime
    dir / materialized paths)
  - the daemon's own canonical checkout
    (`~/.cacophony/daemon/checkouts/cacophony/.cacophony/profiles/`)
  - `~/.cacophony/profiles/`
  - embedded profiles
- For a same-checkout authoring workflow where the operator runs
  `caco config validate --config <checkout>/.cacophony/config.yaml`
  against a repo that is **not** the canonical daemon checkout,
  the just-authored profile file in the sibling
  `.cacophony/profiles/` was invisible to validate, so a
  `agents.persistent.foo.profile: <new-profile>` reference triggered
  the bd-7edbbd "did not match" warning.

## After state

- New helper `discover_config_profile_names_with_source(&config,
  Some(path))` scans the directory containing the explicit config
  file for a sibling `profiles/` subdir and walks one level up
  (the checkout root) for the daemon's checkout-scan helper.
  Best-effort: missing/unreadable directories silently ignored.
- The historic `discover_config_profile_names(&config)` is now a
  thin wrapper that passes `None` for source path, so all
  materialized-snapshot and runtime-dir code paths preserve their
  existing behaviour bit-for-bit.
- Wired the source-aware variant through three call sites that
  already hold the explicit `--config` path:
  - `load_config_for(...)` explicit-override branch
  - `load_validate_and_dual_hash_for_with_warnings(...)`
  - the post-write validate path in `config-edit`
- All other call sites continue using the no-source variant.

## Diff summary

- Commit: c2c84bea4
- Files touched: `crates/caco-cli/src/lib.rs` (+113, -3)
- New unit test:
  `discover_config_profile_names_with_source_finds_sibling_profile_bd_523935`
  materialises a tempdir with
  `.cacophony/profiles/bd523935-test-profile.md` plus a minimal
  `config.yaml` and asserts that passing the explicit config.yaml
  path to the new helper surfaces the sibling profile name.
- Tests: cargo test-small 261/261 pass; the focused new test passes.
- Behaviour delta: only `--config <path>` invocations gain extra
  discovery; runtime-dir / materialized-snapshot paths unchanged.

## Operator-takeaway

`caco config validate --config <checkout>/.cacophony/config.yaml`
now sees profiles authored in `<checkout>/.cacophony/profiles/` in
the same authoring session. The bd-7edbbd warning still fires when
a profile name is genuinely unresolvable — only the same-checkout
false-positive case is closed. Materialized-snapshot validation
(no `--config`) is unchanged: that path continues to scan only
canonical/daemon/user-scoped locations, since materialized
snapshots are derived from the daemon-canonical view by definition.

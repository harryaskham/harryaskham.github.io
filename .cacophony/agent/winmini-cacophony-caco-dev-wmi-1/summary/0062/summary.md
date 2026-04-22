# Session summary — bd-40907c: bootstrap dev help completeness + hello-world honesty

## Goal

Two bootstrap-surface papercuts caught during a
test-user onboarding pass:

1. `caco bootstrap dev --help` only documented `--check`,
   but the error message reveals 5 implemented flags
   (`--init-config`, `--start-daemon`,
   `--create-project NAME`, `--join PROJECT`,
   `--demo-agent`, plus `--force` / `--node-name`).
2. `caco hello-world` hardcoded `node: localhost` and
   `127.0.0.1:12100` listener even though every other
   surface knows the real node identity (e.g. `winmini`)
   and bind host (e.g. `100.124.46.12`). Misleading for
   a first-impression onboarding command — copying the
   listener address into a peer config gets connection
   refused.

## Bead(s)

- `bd-40907c` — P3 bug, test-user-hel filed.

## Before state

```
$ caco bootstrap dev --help
Arguments:
  --check  Run the read-only precondition checklist...

$ caco bootstrap dev
error: requires one of: --check, --init-config,
       --start-daemon, --create-project NAME, --join
       PROJECT, --demo-agent.

$ caco hello-world
node: localhost
listeners: daemon 127.0.0.1:11100 / 127.0.0.1:12100, ...
```

## After state

```
$ caco bootstrap dev --help
Arguments:
  --check         ...
  --init-config   Generate a starter config tree...
  --start-daemon  Start the local caco-daemon...
  --create-project Create a new project entry...
  --join          Join an existing PROJECT...
  --demo-agent    Spawn a single demo agent...
  --force         Used with --init-config: overwrite...
  --node-name     Used with --init-config: explicit local node...

$ caco hello-world
node: winmini
listeners: daemon 100.124.46.12:12100 / 127.0.0.1:12100, ...
```

- `BOOTSTRAP_DEV_ARGS` extended from 1 → 8 entries; the
  branch-summary now reads `Developer-onboarding
  helpers (bd-334962/bd-422b85). One mode per invocation:
  --check / --init-config / ...`.
- `render_bootstrap_payload` now resolves the configured
  local node via `caco_config::resolve_node` +
  `resolve_static_cluster_contract`, gracefully falling
  back to the legacy `localhost` / `127.0.0.1:11100`
  values when no config is loadable (so first-run
  pre-config still gets a sensible payload).
- NodeSnapshot/ListenerPair fields converted from
  `&'static str` to `String`.

## Diff summary

- 1 file touched, +106 / −15:
  - `crates/caco-cli/src/lib.rs`:
    BOOTSTRAP_DEV_ARGS expansion + branch summary;
    NodeSnapshot/ListenerPair owned-string conversion;
    render_bootstrap_payload config-aware resolution.

## Verification

- `cargo build --bin caco`: clean.
- `caco hello-world` on winmini → `node: winmini`,
  `daemon 100.124.46.12:12100 / 127.0.0.1:12100`.
- `caco bootstrap dev --help` lists all 8 flags.

## Operator-takeaway

CLI honesty pass continues — bootstrap surface is the
first impression a new operator gets, so accuracy matters
disproportionately. Two general patterns worth noting:

1. When an error message enumerates a flag set, that
   list IS the de-facto help. Keep them in lock-step
   with the ArgSpec entries (or extract a single source).
2. Hardcoded "localhost"/"127.0.0.1" in any output
   surface is a smell. The fallback path should be a
   last-resort, not the default — config resolution
   already happens elsewhere in the same binary.

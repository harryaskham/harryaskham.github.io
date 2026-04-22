# Session summary — bd-57ac43: bootstrap dev --init-config backup

## Goal

Add a pre-overwrite snapshot to `caco bootstrap dev --init-config`
so an accidental `--force` doesn't silently destroy the existing
`~/.cacophony/config.yaml`. Filed during recovery from a 30-min
daemon-listener outage on ms-dev caused by exactly this footgun.

## Bead(s)

- `bd-57ac43` — caco bootstrap dev --init-config overwrote existing
  config without backup (P1, filed by this session during recovery)

## Before state

- `~/.cacophony/config.yaml` had been overwritten from the
  production import-only sentinel to a `forced-node` default-config
  block (port 11100 instead of cluster-routed 12100), causing `no
  caco-daemon listener configured for node 'ms-dev'` for ~30min on
  every CLI call from agents rooted at `~/.cacophony/`.
- Recovery only worked because `~/.cacophony` happens to be a git
  repo with periodic `bd-5fafbf: config snapshot (daemon startup)`
  commits — far too thin a safety net for a one-flag overwrite of
  the cluster-membership config.
- `--init-config` already refused without `--force` (good), but
  with `--force` it silently clobbered.

## After state

- When `--force` overwrites an existing config,
  `<dir>/config.yaml.bak.<unix-secs>` is written first, byte-for-byte
  copy of the prior contents.
- Backup failure (disk full, permissions) errors out *before*
  touching the real config — refusing to overwrite without a backup
  is safer than overwriting and warning.
- Surfaced through both text mode (`backed up prior config to
  <path> (bd-57ac43)` line) and JSON (`data.backup_path =
  string|null`).
- Refusal-without-`--force` path unchanged (still exits 1, no
  backup file created).

## Diff summary

- Commit: `aa45da1d`
- File: `crates/caco-cli/src/lib.rs` (+153 / -4)
- Tests: +2 (force-snapshots-prior-config; without-force-no-backup
  contract). Both use `CACOPHONY_DIR`-pointed temp dirs.
- `cargo build -p caco-cli` + `cargo clippy -p caco-cli --no-deps`
  clean. Lib test suite green (908 + 2 = 910 tests).

## Operator-takeaway

Recovery from an accidental `caco bootstrap dev --init-config
--force` is now `cp ~/.cacophony/config.yaml.bak.NNN
~/.cacophony/config.yaml` instead of `hope ~/.cacophony is a git
repo with snapshot commits`. Stronger defenses (refuse even with
`--force` when content differs from the default template;
integration test of double-init) are noted as follow-ups; the `.bak`
snapshot is the immediate operator-recovery unblock that closes the
incident class.

## Operations note

This bead was both filed and closed within this session — the
filing was the post-mortem of the 30-min outage, and the fix was
the immediate hardening so the same footgun can't reach a recovery
path again.

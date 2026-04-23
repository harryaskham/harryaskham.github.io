# Session summary — bd-faee97: changelog sensor prefers daemon checkout

## Goal

`caco doctor`'s changelog-up-to-date sensor was producing a
persistent '192 versions behind' false positive on ms-mac that
quietened on other nodes. Make the sensor locality-invariant so
the same daemon version produces the same verdict everywhere.

## Bead(s)

- `bd-faee97` — [operator-action] Release tagging 192 versions
  behind — doctor sensor bd-917f8a fix surfaced drift

## Before state

`locate_repo_changelog` walked parents from `cwd` looking for
any `CHANGELOG.md` + `Cargo.toml` pair, then compared the running
daemon version against that file. On ms-mac, doctor had been
launched from a stale agent checkout whose CHANGELOG ended at
v1.2.329 — running daemon was actually current (v1.2.521+). The
sensor reported 192 versions behind. Other nodes saw nothing
because their cwd happened to resolve to a fresh tree.

The bead's investigation explicitly diagnosed this as
'sensor-design, not actual changelog drift' and recommended the
smallest-diff fix: probe the canonical daemon checkout first,
fall back to cwd-walk only when no daemon checkout is reachable.

## After state

- `locate_repo_changelog` probes
  `~/.cacophony/daemon/checkouts/<project>/CHANGELOG.md` first.
  The daemon checkout is the tree that drives the running daemon,
  so its CHANGELOG is the only one that can plausibly justify a
  'running version is newer than CHANGELOG' warning.
- Falls back to the historical cwd-walk when no daemon checkout
  exists (e.g. operator binary on a node without a daemon).
- Sensor verdict is now the same on every node for the same
  daemon version regardless of where doctor is invoked from.

## Diff summary

- `crates/caco-cli/src/lib.rs` (+153):
  - `locate_repo_changelog`: HOME-driven daemon-checkout probe
    prepended; cwd-walk preserved as fallback.
  - 2 new tests:
    - `locate_repo_changelog_prefers_daemon_checkout_over_cwd`:
      stale cwd CHANGELOG (v0.0.1) + fresh daemon-checkout
      CHANGELOG (v9.9.9) → asserts v9.9.9 is returned.
    - `locate_repo_changelog_falls_back_to_cwd_walk_when_no_daemon_checkout`:
      no daemon checkout → cwd-walk fallback works.
  - Both tests serialise on a static Mutex (HOME + cwd are
    process-global) and restore both in teardown.
- `cargo test -p caco-cli --lib changelog`: 5/5 pass.
- `cargo clippy -p caco-cli`: 2 pre-existing warnings, none in diff.

## Embedded artefacts

(none)

## Operator-takeaway

The bead body explicitly noted "NOT in scope: actual CHANGELOG
backfill" — main was already current through v1.2.527. This is
purely a sensor-locality fix. The persistent ms-mac warning will
clear on the next doctor run after the daemon picks up this
binary. Existing `CACO_DOCTOR_SKIP_CHANGELOG=1` opt-out is
unaffected (early-return guard runs before the probe).

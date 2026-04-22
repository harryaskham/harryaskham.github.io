# Session summary — bd-aa4e94 caco update accepts drafts with matching arch asset

## Goal

`caco update` should consider draft releases that already have the
current platform's asset uploaded, so per-arch independent
publishing (bd-b4a0e1) doesn't block updates while other arches
are still building. Operators retain a `--stable-only` opt-out for
conservative channels.

## Bead(s)

- `bd-aa4e94` — caco update: consider draft releases with matching
  arch asset as update candidates (P1 feature)

## Before state

- `check_for_update` (Stable channel) sorted releases by
  `published_at` desc and walked them looking for the first one
  with a parseable semver tag and the platform's artifact.
- Drafts have null `published_at` and got sunk to the bottom of
  the sort, so they were effectively excluded.
- Result: a node whose arch had already uploaded its artifact had
  to wait for ALL other arches to finish before `caco update`
  could see the new release.
- Failing tests: none (latent — selection rule was implicit).

## After state

- New `--stable-only` CLI flag on `caco update`. Default behaviour
  (flag absent) considers drafts that have the platform's asset.
- New `UpdateCheckOptions { stable_only }` threaded through a new
  `check_for_update_with_options`. The original `check_for_update`
  is retained as a thin wrapper using `UpdateCheckOptions::default()`
  (= stable_only=false), so the daemon-side auto-update path
  (`auto_restart.rs` callers) and update-status fan-out get the
  new asset-first selection rule for free.
- New `select_highest_semver_release_with_asset` walks all releases,
  parses tags as semver, and picks the highest one with the
  matching asset. Used in default mode.
- `select_stable_update_release` in stable_only mode keeps walking
  releases in `published_at` order and excludes drafts explicitly,
  giving operators a clean opt-out.
- New `release_is_draft` helper defensively defaults to false when
  the field is missing, for forward compatibility.
- 7 new unit tests; 3 existing retargeted to use the new options
  type with no semantics change to their fixtures.
- `cargo test-small` green (45).

## Diff summary

- Commit: `31f74555` (rebased onto `f1ef26ab`)
- Files touched:
  - `crates/caco-cli/src/lib.rs`: +340 / −20
    - new `UpdateCheckOptions`, `check_for_update_with_options`
    - new `select_highest_semver_release_with_asset`,
      `release_is_draft`
    - new `--stable-only` flag spec + dispatcher plumbing
    - 7 new unit tests
- Behavioural delta (default mode): selection is now semver-first,
  not published_at-first, and drafts with the matching asset are
  eligible. The auto-update background path picks up the same
  rule because it shares `check_for_update`.
- Behavioural delta (`--stable-only`): identical to pre-bd-aa4e94
  behaviour, with the addition of an explicit `draft: true` filter
  (defensive — pre-bd-aa4e94 relied on sort order alone).

## Operator-takeaway

After this lands, on hosts where the arch artifact has been
uploaded (even into a still-draft release), `caco update` will
offer to install it. For conservative roll-out, set
`--stable-only` or pin the configured channel to a Stable-only
flow that explicitly publishes releases atomically.

The CLI flag also applies to the unattended auto-update path
through the shared `check_for_update` wrapper. If operators want
the unattended path to remain conservative while operator-driven
`caco update` is permissive, that requires plumbing `stable_only`
through `auto_restart.rs` callers — not yet done; file as a
follow-up if needed.

## Embedded artefacts

(none)

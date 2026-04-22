# Session summary — bd-33d37c + bd-8ae8d7: --since validation; sparse checkouts not 'degraded'

## Goal

Two CLI honesty fixes batched in one reintegration:

1. `caco event log --since <bogus>` silently dropped the
   filter and returned the full unfiltered list — same
   shape as bd-c061d4 (`log exceptions --since`).
2. Operator broadcast (ms-mac:operator + caco-ctrl
   relay): `caco doctor` reports `checkout
   'picasso-health' checkout directory does not exist`
   as a warning, but picasso-health is a sparse
   project — non-materialization on a given node is
   expected, not a degradation.

## Bead(s)

- `bd-33d37c` — own bead filed when self-sourcing the
  `--since` family. Closed.
- `bd-8ae8d7` — P2 task from operator broadcast
  ("do not consider sparse checkouts for projects as
  broken"). Closed.

## Before state

```
$ caco event log --since notreal
56 event(s):              # full unfiltered list
  ...

$ caco doctor | grep picasso
! warning  checkout 'picasso-health'  checkout directory does not exist
```

`compute_checkout_health` (daemon-side, used by
`/api/v1/beads/status` and standby-candidate scoring)
also returned `blocker = "beads path does not exist: ..."`
for sparse-configured projects with no local materialization.

## After state

```
$ caco event log --since notreal
error: invalid --since value 'notreal' (expected e.g.
3h, 30m, 1d or RFC 3339 timestamp)

$ caco event log --since 5m
No command events recorded.

$ caco doctor | grep picasso
? info  checkout 'picasso-health'  sparse-configured
checkout not materialized on this node (expected)
```

`compute_checkout_health` consults
`state.config.projects[*].sparse.is_some()`; if true,
absence of the beads path no longer sets a blocker.
Other blocker classes (not-a-git-checkout,
sync=degraded/failed) still apply.

## Diff summary

- 2 files touched, +49 / −7:
  - `crates/caco-cli/src/lib.rs`:
    - `dispatch_event_log`: validate `--since` against
      either `parse_since_duration` OR
      `chrono::parse_from_rfc3339` (mirroring the
      daemon's `parse_since` shape).
    - Doctor section 9: missing-checkout branch checks
      `project.sparse.is_some()`; emits
      `info` with explicit phrasing for sparse projects.
  - `crates/caco-daemon/src/beads.rs::compute_checkout_health`:
    sparse-aware blocker computation; absent beads path on
    a sparse-configured project is no longer a blocker.

## Verification

- `cargo build --bin caco`: clean.
- `caco event log --since notreal` → error (was: full list).
- `caco event log --since 5m` → empty result page (parses).
- `caco doctor | grep picasso` → info row, not warning.

## Operator-takeaway

- bd-33d37c is the 7th in the silent-unknown-value family
  (bd-126b99/bd-a403a1/bd-30fbfb/bd-2886bb/bd-dfc91a/bd-bc52ef
  + bd-c061d4 + bd-3656ce + bd-40907c are siblings).
  Operator-takeaway from bd-bc52ef summary 0061 was:
  "if a 7th appears worth extracting `validate_enum_flag`
  helper". For `--since` specifically, a
  `validate_since_or_rfc3339(s) -> Result<(), CliError>`
  helper would deduplicate caco event log + caco log
  exceptions + caco agent logs callsites — filing as
  follow-up if a 4th `--since` site shows up.
- bd-8ae8d7: pairs with bd-c5b... (operator's tracking
  bead) and the project-controller "Node-Health Noise
  Calibration" pattern. Sparse-checkout suppression is
  done at two layers (CLI doctor + daemon
  compute_checkout_health); narrator/cluster-ctrl
  surfaces that consume `/api/v1/beads/status` will
  inherit the daemon-side fix automatically.

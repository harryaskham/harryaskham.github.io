# Session summary — bd-b9eccd: caco ls + ps --project converge on gold-standard security-WHY validator

## Goal

Resolve the within-CLI `--project` validator drift identified
in bd-b9eccd Issue 4, where:

- `caco build list --project bogus` and
  `caco changelog show --project bogus` use the
  gold-standard security-WHY wording.
- `caco fleet snapshot --projects bogus` uses
  inline-allowed-values wording.
- `caco ls --project bogus` and `caco ps --project bogus`
  silently accepted any value (now fixed via bd-2dc0c3 +
  bd-bc3d7d, but with their own ad-hoc wording).

This bead converges the ls/ps wording onto the gold-standard
established by bd-6dc352 (security-WHY + Configured: list).

## Bead(s)

- `bd-b9eccd` — `caco ls + ps SILENT-ACCEPT MASTERCLASS — 7
  filter flags ALL silently accept any value while sister
  surfaces gold-standard validate; --project DRIFT
  (build/changelog gold-standard with security-WHY but
  ls/ps silently accept) — supposedly-shared validator NOT
  actually shared`.

## Before state

Issues 1 + 2 (silent-accept on ls/ps filters) were already
fixed earlier in this drain via bd-2dc0c3 (caco ps) and
bd-bc3d7d (caco ls), but with bespoke per-surface wording:

- `caco ls --project bogus` →
  `error: caco ls: project 'bogus' is not configured.
  Configured projects: ...`
- `caco ps --project bogus` →
  `error: caco ps: project 'bogus' is not configured.
  Configured projects: ...`

vs gold-standard (build, changelog, etc.):

- `caco build list --project bogus` →
  `error: project 'bogus' is not configured; bead
  operations must target a configured project to prevent
  routing to an ambient external board. Configured: a, b, c`

Issue 4 of bd-b9eccd correctly called this out as a
within-CLI cohort split.

## After state

`dispatch_ps` and `dispatch_ls` `--project` validators now
emit the exact gold-standard wording:

```
project 'bogus' is not configured; bead operations must
target a configured project to prevent routing to an
ambient external board. Configured: a, b, c
```

(or `Configured: (none)` when no projects are configured).

This makes the cluster's `--project` not-configured error
identical across:

- `caco build list / show / cancel`
- `caco changelog show`
- `caco bd list / show / claim / ...` (via
  resolve_project)
- `caco test list / show / run`
- `caco release list / show / status`
- `caco summary` (via resolve_project)
- `caco ls`            ← landed in this bead
- `caco ps`            ← landed in this bead

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `dispatch_ps`: replaced inline `--project` validator
    error wording with the gold-standard security-WHY +
    Configured: variant.
  - `dispatch_ls`: same.
  - Updated source-grep tests
    `dispatch_ps_validates_kind_state_and_project_filters`
    and `dispatch_ls_validates_kind_project_and_agent_filters`
    to match the new wording (substring
    `"project to prevent routing to an ambient external
    board"`).
- `cargo test -p caco-cli --lib -- ...`: all 3 affected
  tests pass (ps + ls validators + the
  resolve_project_error_lists_configured_projects gate
  from bd-6dc352).
- `cargo test-small`: 180 pass.

## Operator-takeaway

The `--project` not-configured validator is now genuinely
shared across the CLI. The remaining inconsistency is
`caco fleet snapshot --projects bogus` which uses
inline-allowed-values wording — that's a different
**flag** (`--projects`, plural) for the multi-project
fan-out, and a different code path; left alone here
because converging it would change a long-stable error
shape that may break existing scripts.

The other issues in bd-b9eccd are positive observations
(Issues 3, 6, 7 — already-existing gold-standards) or
already-shipped fixes (Issues 1, 2 — bd-2dc0c3 +
bd-bc3d7d). Issue 5 (caco ls --json half-flat envelope:
`{ok, entries, count, runtime_root, node}` no `data`
wrapper) is a wire-format change that would break
existing JSON consumers — same risk profile as the
caco summary `--json` envelope question in bd-7abbba.
Worth its own coordinated bead for a cluster-wide
envelope-shape rollout, not a point fix here.

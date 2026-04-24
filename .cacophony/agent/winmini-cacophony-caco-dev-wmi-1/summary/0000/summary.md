# Session summary — bd-925e1b: caco project namespace envelope unification

## Goal

Unify the three divergent JSON envelope shapes inside the `caco project`
namespace (`list`, `status`, `show`) onto the standard `{ok, data,
meta}` envelope catalogued under bd-5ae1ce. Worst intra-namespace
divergence yet — three sister surfaces, three different shapes.

## Bead(s)

- `bd-925e1b` — caco project namespace has THREE distinct JSON envelope
  shapes across sister surfaces (P3 bug). Issue 1 fixed; Issue 2
  (--name vs --project) and the FAILED-counter rollup gap left for a
  follow-up.

## Before state

```
$ caco project list   --json | jq 'keys'   # shape A
["count", "ok", "projects"]
$ caco project status --json | jq 'keys'   # shape B
["count", "daemon_reachable", "ok", "projects"]
$ caco project show   --json | jq 'keys'   # shape C
["ok", "project"]
```

Three shapes; none used the cluster-standard `{ok, data, meta}`
envelope. `count` and `daemon_reachable` were hoisted to top-level
on list/status; show used a singular `project` key with no envelope
at all.

## After state

All three return the same shape:

```
$ caco project list   --json | jq 'keys'
["data", "meta", "ok"]

$ caco project status --json | jq 'keys'
["data", "meta", "ok"]    # data.projects, meta.{count, daemon_reachable}

$ caco project show   --json | jq 'keys'
["data", "meta", "ok"]    # data.project, meta.source = "daemon"|"config"
```

- `list`: payload moves to `data.projects`; `count` moves under `meta`.
- `status`: payload moves to `data.projects`; both `count` and
  `daemon_reachable` move under `meta`.
- `show`: both daemon-backed and config-fallback paths wrap their
  payload under `data.project` and add `meta.source` =
  `"daemon"|"config"` so consumers can tell which read model
  produced the result.

## Diff summary

- 1 file changed, +30 / -10 (`crates/caco-cli/src/lib.rs`).

## Validation

- `cargo check -p caco-cli --tests`: clean.
- No existing tests assert on the old shapes (grep'd
  `crates/caco-cli/tests/` and root `tests/`).

## Operator-takeaway

- Cross-surface envelope catalogue: three project-namespace variants
  collapse into one shape. Estimated catalogue count drops by 2.
- **BREAKING for any script reading**:
  - `caco project list   --json | jq .count`        → `.meta.count`
  - `caco project list   --json | jq .projects`     → `.data.projects`
  - `caco project status --json | jq .daemon_reachable` →
    `.meta.daemon_reachable`
  - `caco project status --json | jq .projects`     → `.data.projects`
  - `caco project show   --json | jq .project`      → `.data.project`
- `meta.source` on `project show` is new (no previous field).
- Issue 2 (`--name` typo silently empty) and the FAILED-counter
  rollup gap remain open — if they prove worth a separate bead, can
  be filed as `bd-925e1b` children.

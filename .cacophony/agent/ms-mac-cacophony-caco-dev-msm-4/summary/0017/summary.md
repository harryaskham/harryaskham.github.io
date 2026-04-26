# Slice 15 — bd-377c4b sisters: collapse two more bare-flat JSON shapes

## Goal

Collapse two residual bare-flat JSON envelope surfaces flagged by test-user-hel as in-family with the bd-377c4b/bbcc36/548e77/3e39a0 envelope sweep.

## Bead(s)

- Sister-pattern continuation of **bd-377c4b** (joined family, no new bead per same-root-cause workflow).

## Surfaces collapsed

1. `caco bd snapshot list --json`
   - **before:** `{count, entries, ok, project, root}` (bare-flat with ok)
   - **after:** `{ok, data:{entries}, meta:{project, root, count}}`
2. `caco log exceptions --json`
   - **before:** `{node, exceptions, total, component_filter, pre_component_total, after_component_total, returned}` (bare-flat without ok)
   - **after:** `{ok, data:{exceptions}, meta:{node, total, component_filter, pre_component_total, after_component_total, returned}}`

## Before state

- 17 distinct envelope shapes catalogued cluster-wide (per test-user-hel).
- Bare-flat family was at 5+ surfaces post-bd-377c4b.

## After state

- Bare-flat family reduced by 2.
- Both surfaces now match the canonical `{ok, data, meta}` shape used by tts/choices/notify/event/timeline/bd-reconcile-log/cert.

## Diff summary

```
 crates/caco-cli/src/lib.rs        | ~10 lines (bd snapshot list)
 crates/caco-cli/src/outbox_cmd.rs | ~10 lines (log exceptions)
```

`cargo check -p caco-cli` clean. No existing tests assert on either old shape.

## Operator-takeaway

Two more JSON-envelope drift surfaces collapsed into the canonical shape — scriptable consumers can rely on `.data.entries` / `.data.exceptions` consistently.

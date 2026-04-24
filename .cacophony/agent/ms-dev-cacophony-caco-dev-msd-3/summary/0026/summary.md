# Session summary 0026 — bd-c5fa50: doctor stack-overflow + lifecycle-area renderer bug

## Goal

Address the two pre-existing doctor_includes_* test failures
po4-5 flagged in bd-c5fa50 as still-broken-on-main after the
config_distribute siblings landed.

## Bead(s)

- `bd-c5fa50` — [broken-on-main] config_distribute_with_distribute_command_node_uses_command stack overflow (sibling: doctor_includes_*)

## Before state

- `cargo test -p caco-cli --lib doctor_includes_snapshot_pinned_count_sensor` → stack overflow.
- `cargo test -p caco-cli --lib doctor_includes_lifecycle_supervisor_section` → stack overflow.
- po4-5's bd note said the second one "needs more than a stack bump" because dispatch_doctor returns Err in the test env. Investigated below.

## After state

Two fixes in one commit:

1. **Stack-bump for both doctor_includes_* tests.** Same pattern
   already used for config_distribute_* and 3 other tests at
   lib.rs:92702/92725/92747: spawn the test body in a thread with
   `RUN_DISPATCH_STACK_SIZE` (16 MB).

2. **Renderer bug fix.** Once the second test could actually run,
   it failed because the bd-4acdd7 author added `area: "lifecycle"`
   to the native-supervisor check at lib.rs:51961+ but never
   registered `"lifecycle"` in the renderer's `areas` array at
   lib.rs:52915. Result: every lifecycle check was silently dropped
   from the rendered output — the operator's `caco doctor` has been
   hiding native-supervisor status on every node since bd-4acdd7
   landed. Slot `"lifecycle"` between `"services"` and `"mesh"`.

## Diff summary

- `crates/caco-cli/src/lib.rs`: +53 lines / -2.
  - 2 thread-spawn wrappers + `_inner` extraction.
  - 1 `areas` array entry.
- `cargo test -p caco-cli --lib doctor_includes_`: 2/2 pass.
- `cargo build -p caco-cli`: clean.

## Embedded artefacts

(none)

## Operator-takeaway

The lifecycle-area-not-registered bug is the more interesting find.
`caco doctor` on every node has been silently hiding the
native-supervisor status since bd-4acdd7. Worth a sweep:

```sh
grep -nE 'area:\s*"[a-z_]+"' crates/caco-cli/src/lib.rs \
  | sed -E 's/.*area:\s*"([a-z_]+)".*/\1/' | sort -u
```

against the `areas` literal at the renderer to detect any other
silently-dropped DoctorCheck areas.

bd-c5fa50 will remain in_progress this session; will reintegrate +
update bead status next.

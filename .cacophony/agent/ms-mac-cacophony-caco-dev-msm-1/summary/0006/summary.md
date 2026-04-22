# Session 0006 — bd-517e52

## Goal

First-slice implementation of duplicate-of bead linking, deferred from
the broader spec into a CLI-only composition that ships now and paves
the way for a richer schema-level model later.

## Bead(s)

- bd-517e52 — claimed and worked end-to-end. First slice; richer model
  (BeadStatus::Duplicate variant, duplicate_of FK column, reverse-lookup
  on `bd info`, `bd create --check-duplicates`) deliberately deferred to
  follow-up beads to keep this change surgical.

## Before state

No first-class way to express "this bead is a duplicate of bd-XXX,
absorb its context." Operators were closing duplicate beads manually
and either dropping context or hand-pasting it across.

## After state

`caco bd update --bead-id bd-DUP --duplicate-of bd-CANON` now:

  1. Appends a `Merged duplicate: bd-DUP <title>` addendum to bd-CANON's
     description (target absorbs the duplicate's context).
  2. Closes bd-DUP and appends a `Duplicate of bd-CANON` marker line to
     its description so the link is greppable both ways.

Validation:
  - Refuses to combine with other field flags (--title, --status, ...).
  - Rejects self-reference.
  - Rejects targets whose status is closed/deleted.
  - Idempotent on re-run (detects existing markers).

`cargo test-small`: 52/52 PASS. `cargo clippy --workspace --all-targets
-- -D warnings`: clean.

## Diff summary

```
crates/caco-cli/src/lib.rs                | +266 -1
.cacophony/agent/.../summary/0006         | (new)
```

Inside that diff:
  - BD_UPDATE_ARGS: added `--duplicate-of` ArgSpec.
  - dispatch_bd_update: early branch routing to new helper, with
    field-conflict / self-reference guards.
  - dispatch_bd_update_duplicate_of: new helper, two GETs + two PATCHes,
    idempotent, status-validated.
  - tests: 2 new + 1 extended (help-json assertion).
  - bd-c5783b broken-on-main fix: added `short_name_strategy: None` to
    test-fast-gate Profile fixture (~line 74918). Spoke ownership before
    editing.

## Operator-takeaway

For operator triage workflow:
  `caco bd update --bead-id bd-deadbe --duplicate-of bd-canonical`
closes the duplicate, copies its description into the canonical bead's
description as a clearly delimited section, and stamps both ends so a
later `grep` for either ID surfaces the link. No schema work needed.

## Coordination

- Spoke claim of bd-517e52 before starting.
- Spoke `[broken-on-main]` notice before fixing the
  `short_name_strategy` field on the test-fast-gate fixture.
- Will speak completion + reintegrate before picking next bead.

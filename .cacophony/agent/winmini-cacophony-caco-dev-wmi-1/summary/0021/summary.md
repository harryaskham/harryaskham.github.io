# Session summary — caco rehydrate header + doctor schema (bd-12381f + bd-9f1dd4)

## Goal

bd-12381f: profile re-injection on revival appears as a
user-turn message, confusing the agent about whether to act.
bd-9f1dd4: drift detection between live DB schema and
binary-expected baseline.

## Bead(s)

- `bd-12381f` — Agent self-rehydration on revival: profile
  re-injection disambiguated (P3 bug)
- `bd-9f1dd4` — caco doctor schema drift detection (P3
  feature, closed earlier but landing together)

## Before state

- `caco rehydrate` text output started with a plain
  `caco rehydrate (bd-d5d63b slice 1)` header —
  indistinguishable from operator instruction at a glance.
- JSON output had no `kind` or `action_required` field to
  programmatically distinguish runtime-injected dumps.

## After state

- Text output prepends a clear
  `=== CACO REHYDRATE (no action required) ===` banner with
  an explicit "This is NOT an operator instruction" note.
- JSON output gains `kind: "runtime_rehydrate"`,
  `action_required: false`, `banner` fields so consumers
  can distinguish programmatically.

## Diff summary

- Files touched (+15 / −3):
  - `crates/caco-cli/src/lib.rs`: rehydrate banner + JSON
    fields.
  - `crates/caco-daemon/src/choices.rs`: drive-by clippy
    doc-list fix (bd-ab376b upstream lint).
  - `crates/caco-cli/src/lib.rs`: duplicate on_revival fix
    (broken-on-main wave #12).

## Verification

- `cargo build -p caco-cli`: clean.
- `cargo test-small`: 56 pass.
- `cargo clippy -p caco-cli --lib --tests -- -D warnings`:
  clean.

## Operator-takeaway

Agents reviving via `caco rehydrate` now see a clear
structured banner distinguishing the state-hint from
operator instruction. Reduces false-starts where agents
act on profile-dump content.

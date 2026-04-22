# Session summary — bd-b655b6: ChoiceOption.recommended

## Goal

When an agent presents a multiple-choice question to the operator,
let it also flag which option IT recommends if forced to act. Add
the data carrier, surface it in `caco choices show`, and let
`caco choices resolve --use-recommended` accept the agent's
suggestion in one keystroke.

## Bead(s)

- `bd-b655b6` — `Caco choices: include 'recommended' field on choice
  array so operator sees suggested default`

## Before state

- `ChoiceOption { label, summary }` had no way to mark a preferred
  option. Operators had to read all summaries to figure out which
  the agent thought was best.
- `caco choices resolve` required `--selected-index <N>` (or
  `--freeform-text`); no shorthand for "do what you suggested."

## After state

- `ChoiceOption` gains `pub recommended: bool` with
  `#[serde(default, skip_serializing_if = ...not::not)]`. Old
  payloads without the field deserialize to `false`; payloads with
  `recommended:false` omit the field on serialize so cross-version
  wire traffic stays clean.
- `caco choices show` prefixes recommended option(s) with `*` and
  non-recommended with a leading space, so columns align and the
  operator scans 5 choices in O(1) for the agent's pick.
- `caco choices resolve --use-recommended` GETs the choice, finds
  the first option with `recommended: true`, and submits a resolve
  with that `selected_index`. Errors loudly if no option is flagged
  rather than silently picking index 0. Mutually exclusive with
  `--selected-index` and `--freeform-text` — combining would hide
  ambiguity.

## Diff summary

- Commit: `c2dfb9ad`
- Files (4):
  - `crates/caco-daemon/src/choices.rs` — schema + 1 round-trip test
  - `crates/caco-daemon/src/operator_inbox.rs` — test fixtures
    backfilled
  - `crates/caco-daemon/src/lib.rs` — 2 production `ChoiceOption{}`
    sites (Approve/Reject for persistent-spawn-confirmation choice)
  - `crates/caco-cli/src/lib.rs` — show-star + `--use-recommended`
    resolve mode + dispatch wiring
- 1 new lib test (`choice_option_recommended_is_optional_and_defaults_to_false`)
  passes; `cargo test -p caco-daemon --lib choices` still green
  (16 tests); `cargo clippy -p caco-daemon -p caco-cli --no-deps`
  clean.

## Out of scope (informal follow-ups)

- TUI star rendering in `ChoicesTuiState` — schema change flows
  through cleanly; visual is a separate polish bead.
- Web UI badge — same shape, separate bead.
- Validation that at most one option carries `recommended:true` —
  current schema is permissive; `--use-recommended` documents the
  first-wins semantics.

## Operator-takeaway

When cluster-ctrl or any other agent presents 5 options and one is
flagged recommended, operators see `*` in `caco choices show` and
can accept with `caco choices resolve --use-recommended` instead of
reading every summary and remembering an index. Audit trail still
records which option was chosen.

## Cross-bead note

The workspace currently has an unrelated `bd-1ac1b7` broken-on-main
in `caco-cli` lib tests (missing `on_revival` profile field on a
test fixture) — fix-forward owned by another agent. Did not block
my own crate-scoped tests / clippy.

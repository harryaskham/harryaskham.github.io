# Summary 0023 — bd-1c98d0: choice-resolution inbox surface

## Bead
bd-1c98d0 (P2, bug) — "Resolved choice does not notify presenting
agent — requires manual `caco choices show` poll."

## Background
The daemon already delivers choice resolutions to the presenting
agent via `deliver_direct_message` (bd-971e37, in
`crates/caco-daemon/src/choices.rs::handle_resolve_choice`). The
sender is `<node>:daemon:choice-resolution` and the body is a JSON
blob `{"type":"choice_resolution", "choice_id", "selected_index",
"selected_label", "freeform_text"}`.

Issue: `caco msg inbox` rendered this as
`[direct] <node>:daemon:choice-resolution: {"type":"choice_resolution",...}` —
visually opaque JSON in the same column as ordinary text, easy to
miss while skimming. That's exactly what bit cluster-ctrl tonight
(6 minute latency on a P1 disk threshold).

The push is there; the surface was illegible.

## Change

### CLI: friendlier inbox tag for choice-resolution

`crates/caco-cli/src/lib.rs`:

- New `format_inbox_line(&serde_json::Value) -> String` helper
  wraps the per-message rendering. Detects bodies that parse as
  `{"type":"choice_resolution", ...}` and renders them as:
    `[choice-resolved] <sender>: choice=<id> -> <label>`
  or, when only freeform was given:
    `[choice-resolved] <sender>: choice=<id> freeform=<text>`
  Falls back to `[<kind>] <sender>: <body>` for everything else
  (zero behaviour change for non-choice messages).
- `dispatch_msg_inbox` swapped its inline closure for
  `.map(format_inbox_line)` so the formatter is reusable + unit-
  testable.

### Tests

`crates/caco-cli/src/lib.rs::tests` — 4 new unit tests:
- `format_inbox_line_tags_choice_resolution_with_label`
- `format_inbox_line_tags_choice_resolution_with_freeform`
- `format_inbox_line_falls_back_to_legacy_for_normal_messages`
- `format_inbox_line_handles_non_json_body` (defensive)

## What this does NOT change

- The push surface: `deliver_direct_message` already runs on
  resolve; no daemon changes needed.
- The wire format: bodies remain machine-readable JSON so
  programmatic consumers (rehydration, msg snapshot, msg history)
  see exactly what they did before.
- Any other inbox rendering in the TUI / web UI — those have
  their own paths and can be updated separately if needed.

## Operational impact

When cluster-ctrl polls `caco msg inbox` and an operator just
resolved a choice, the line now reads:
  `[choice-resolved] helsinki:daemon:choice-resolution: choice=choice-019db420 -> reduce-pool`
instead of:
  `[direct] helsinki:daemon:choice-resolution: {"type":"choice_resolution","status":"resolved","choice_id":"choice-019db420",...}`

That single visual change closes the bd-1c98d0 latency loop —
the presenting agent's polling code can grep for `[choice-`
instead of trying to JSON-parse every direct message.

## Verification

- `cargo test -p caco-cli --lib format_inbox_line`: 4/4.
- `cargo test-small`: all green (4253/4253 incl new tests).
- `cargo clippy --workspace --no-deps`: zero warnings (had to
  swap `|m| format_inbox_line(m)` for the bare fn pointer to
  satisfy redundant_closure).

## Companions

- bd-020bc1 (msd-4 last cycle): `caco bd operator-actions`
  surface — same family of "make the implicit explicit" CLI work.
- bd-971e37 (already landed): the daemon-side delivery this
  surface change rides on top of.
- bd-939541 (open): operator-ping-beyond-speak; complementary
  push-surface work for operator → agent direction.

## Deferred

- TUI inbox view + web inbox view recognition of
  `[choice-resolved]`: client-side changes only; deferred to
  follow-up beads. The text CLI is the highest-value surface
  because that's what persistents typically poll programmatically.
- Adding a dedicated `MessageKind::ChoiceResolution` enum variant:
  more invasive (DB schema + serde + every kind-filter site) and
  not required to close this bead. The body-tagged path already
  carries the type info; the new line renderer surfaces it.

## Next

Reintegrate, close bd-1c98d0, idle.

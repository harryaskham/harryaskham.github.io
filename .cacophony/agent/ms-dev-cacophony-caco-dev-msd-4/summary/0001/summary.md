# Session summary — bd-3a6078 caco agent emotion/annotate/get UX

## Goal
Surface agent ID + field name in text mode; accept --agent-id alias.

## Bead(s)
- `bd-3a6078` — caco agent emotion/annotate/get text loses metadata; --agent-id rejected

## Before state
- Text rendered raw value (`<unset>`); no agent-ID context.
- `--agent-id` warned + ignored; muscle-memory spelling rejected.

## After state
- Text mode prints `agent: <id>\n<field>: <value>` from JSON envelope.
- `--agent-id` accepted as alias by resolve_agent_id; --id wins when both supplied; error message names both forms.
- AGENT_GET_ARGS, AGENT_EMOTION_*, AGENT_ANNOTATE_* register --agent-id so bd-b76723 warning doesn't fire.
- 3 new tests pin alias / precedence / error message.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+100 / -13)
- `cargo test-small`: 162 passing.

## Operator-takeaway
`caco agent annotate` now shows which agent resolved; `--agent-id` works alongside `--id`. Pattern reusable for sister surfaces in bd-2a4552 / bd-53e157.

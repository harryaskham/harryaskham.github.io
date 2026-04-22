# Session 0005 — bd-ac9289

## Goal

Fix caco-web Choices Resolve button always 404'ing.

## Bead(s)

- bd-ac9289 — claimed and worked end-to-end.

## Before state

`crates/caco-web/static/app.js::resolveChoice` built the URL as
`/api/v1/choices/${choiceId}/resolve` and POSTed `{ selected_index }`.
The daemon route table registers `/api/v1/choices/resolve` (no per-id
path). Every Resolve click → 404 → 'Failed: HTTP 404' toast → choice
never advanced. Android companion already used the correct shape.

## After state

- `crates/caco-web/static/app.js`: URL is `/api/v1/choices/resolve`,
  body carries `{ choice_id, selected_index }`.
- `crates/caco-web/src/tests.rs::app_js_has_choices_logic`: now asserts
  canonical URL present, `choice_id` in body, broken per-id URL form
  absent.
- `cargo test -p caco-web --lib`: 52/52 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

```
crates/caco-web/src/tests.rs    | +15
crates/caco-web/static/app.js   |  +5 -2
.cacophony/agent/.../summary/0005 | (new)
```

## Operator-takeaway

Choices tab on caco-web should now actually resolve choices. Wire shape
matches android companion so both surfaces share one daemon contract.

## Coordination

- Spoke claim of bd-ac9289 before starting; will speak completion + 
  reintegrate before picking next bead.

# Session 0011 — bd-4cbb82

## Goal

Make the caco-web Dispatch button stop 422'ing on every click.

## Bead(s)

- bd-4cbb82 — claimed and worked end-to-end.

## Before state

`crates/caco-web/static/app.js::dispatchBead` and `batchDispatchBeads`
were POSTing `JSON.stringify({})` to
`/api/v1/projects/<p>/beads/<id>/dispatch`, but the daemon's
`QueueDispatchRequest` (`crates/caco-daemon/src/beads.rs`) requires
`{ target_node, payload }`. Every Dispatch click — both the bead-detail
modal and the unassigned-row quick action — returned 422 with
`Failed to deserialize the JSON body` and the pre-claim / spawn-agent
workflow was non-functional on web. The confirm copy also lied
(`This will claim the bead and spawn a new agent`).

## After state

- `dispatchBead` now prompts the operator for a target node from the
  snapshot's `configured_nodes`, POSTs the canonical
  `{ target_node, payload: {} }`, and rephrases the confirm copy as
  "Queue bead X for dispatch on '<node>'?".
- `batchDispatchBeads` does the same thing — picks one target node
  once and applies it to every selected bead.
- New `pickDispatchTargetNode` helper hosts the picker logic with a
  sane free-form fallback when the snapshot list is empty.
- `crates/caco-web/src/tests.rs::app_js_dispatch_button_uses_canonical_request_shape`
  locks the wire shape (function presence, `target_node` and `payload`
  in body, no empty-object POST to `/dispatch`).

Inline broken-on-main fix (spoke ownership before edit):
- `crates/caco-daemon/src/lib.rs:23743` — `explicit_counter_loop` in
  the daemon-side `caco agent log` handler (bd-83a84d), rewritten as
  `raw.lines().take(*k)`. Same fix as my bd-2977fc commit ~1h ago,
  this time on the daemon path that landed via msd-4 in between.

Validation:
- `cargo test -p caco-web --lib`: 54/54 PASS.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean.

## Diff summary

```
crates/caco-web/static/app.js          | +44 -10  (handler rewrite + helper)
crates/caco-web/src/tests.rs           | +29
crates/caco-daemon/src/lib.rs          |  +6 -10  (explicit_counter_loop)
.cacophony/agent/.../summary/0011      | (new)
```

## Operator-takeaway

caco-web Dispatch button now actually queues a dispatch instead of
spamming 422 toasts. Both single-bead and batch flows work. Operator
gets prompted for the target node — the auto-pick UX is left as a
follow-up.

## Coordination

- Spoke claim of bd-4cbb82.
- Spoke `[broken-on-main]` ownership for the daemon-side
  explicit_counter_loop fix (mirrors bd-2977fc CLI-side fix earlier).
- Will speak completion + reintegrate.

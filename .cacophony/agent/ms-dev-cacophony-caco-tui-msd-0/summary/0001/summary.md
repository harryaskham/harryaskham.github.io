# Session summary — not-spawned persistent stable-id (TUI + cross-node resolve)

## Goal

Finish the bd-e62b61 tail: make a NEVER-spawned (`auto_start:false`) persistent's
TUI lifecycle actions target the stable managed agent-id instead of the random
`persistent-<hash>` id, and extend the daemon's cross-node persistent resolver
so the Phase-2 live-probe path also accepts the stable id. Both were cosmetic/
edge (the random-id path works via accept-either) but left the not-spawned
lifecycle URL showing `persistent-<hash>`.

## Bead(s)

- `bd-741c6d` — bd-e62b61 tail: not-spawned TUI stable-id data-model + Phase-2
  dynamic-node resolve edge (task, P3).
- (parent: `bd-e62b61` — stable managed agent-id lifecycle machinery)

## Before state

- Failing tests: none.
- The previous investigator (aur-4) unclaimed this, believing PART 1 needed a
  NEW `stable-agent-id` field threaded into `PersistentAgentSnapshot`
  (ui_stream.rs) because they "could not locate a single reusable derivation"
  for the stable id.
- TUI `request_persistent_start/recreate/stop` fell back to the raw
  `persistent_id` when `backing_agent_for_persistent` returned None.
- Daemon `resolve_remote_persistent_node` Phase 1 matched the stable id but
  Phase 2 (live-probe of dynamic/ACA nodes) matched only the exact
  `persistent_id`.

## After state

- Failing tests: none. `cargo check -p caco-tui -p caco-daemon --tests` passes;
  3 targeted tests pass (1 daemon predicate + 2 TUI derivation/lookup).
- De-risked the blocker: the stable id is `{node}-{project}-{decl_name}` (or
  `{node}-{decl_name}` node-scoped), confirmed by the accept-either resolver
  (`resolve_lifecycle_persistent_id`) + a passing test, and is already derivable
  TUI-side from `PersistentAgentDisplayState` (node + project + decl_name). No
  daemon snapshot schema change was needed.
- PART 1 (TUI-only): added `PersistentAgentDisplayState::stable_agent_id()` +
  `TuiState::stable_agent_id_for_persistent()`, and the 3 lifecycle fns now
  `.or_else(stable_agent_id_for_persistent)` before the raw-id fallback.
- PART 2 (daemon): extracted `persistent_lifecycle_id_matches()` and used it in
  BOTH Phase 1 and the Phase 2 snapshot-probe closure, so dynamic/ACA
  persistents resolve by stable id too.

## Diff summary

- Code commit: created at reintegration; final landed squash SHA from the
  reintegration receipt.
- Files touched: `crates/caco-tui/src/state/mod.rs` (stable_agent_id method +
  state helper), `crates/caco-tui/src/app.rs` (3 lifecycle fallbacks),
  `crates/caco-tui/src/views/agent_detail.rs` (2 tests),
  `crates/caco-daemon/src/lib.rs` (shared predicate + Phase 1/2 use + 1 test).
- Tests: +3. Behavioural delta: not-spawned persistent lifecycle URLs use the
  stable id; cross-node Phase-2 resolve accepts the stable id.

## Operator-takeaway

The previous investigator's blocker was illusory: the stable-id derivation
(`{node}-{project}-{decl_name}`) already existed implicitly in the resolver and
the TUI display state, so no daemon schema change was required. The cost was
that the derivation isn't a single discoverable named helper — it's re-derived
ad hoc in several places. Filed a reflection draft to extract it into one
shared `caco-config` helper so the next person doesn't re-derive or give up.

# Session summary — bd-b357f0: widget_placements stale-wasm fix + live test

## Goal

Re-add the live widget-placement subscenario (now possible because bd-04d343
made the main pico-pane scenario deterministic) — and in doing so, discover and
fix that the bd-02730e widget_placements feature was silently INCOMPLETE
end-to-end because the committed wasm was stale.

## Bead(s)

- `bd-b357f0` — widget_placements dropped end-to-end by stale wasm (bd-02730e incomplete); regen wasm + live test
- Follows `bd-02730e` (JS routing) and `bd-04d343` (deterministic scenario, which unblocked the live test).

## Before state

- Failing tests: none (but the feature was broken end-to-end).
- bd-02730e added the JS routing (renderPicoFooter reads snap.widget_placements)
  with only a STATIC source guard. The committed pico_view_bg.wasm was stale
  relative to bd-eb071d (which added widget_placements to the shared
  AgentViewSnapshot): `strings pico_view_bg.wasm | grep widget_placements` => 0.
  So the wasm snapshotJson dropped the field and belowEditor widgets fell back to
  the footer.

## After state

- Failing tests: none.
- Regenerated pico_view_bg.wasm (`strings ... | grep widget_placements` => 2), so
  the placement hint now reaches the renderer. Re-added the live caco-web-observe
  widget-placement subscenario.
- caco-web `--lib` 649; bin 12; clippy clean. Live pico-pane widget subscenario
  2/2 clean: `belowHasTokens=true, footerLacksTokens=true, belowHidden=false`
  (was `belowHasTokens=false, footerLacksTokens=false` before the regen).

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/pico_view_bg.wasm` — regenerated (now serializes widget_placements).
  - `crates/caco-web/src/bin/caco-web-observe.rs` — live widget-placement subscenario.
- Tests: +1 live subscenario; the static guard from bd-02730e remains.
- Behavioural delta: belowEditor widgets now actually render beneath the composer.

## Embedded artefacts

- `web/widget-live-run.log` — the clean live run after the wasm regen.

## Operator-takeaway

A static source guard proves the JS code exists but NOT that the shared wasm
actually carries a new snapshot field — the bd-02730e guard passed while the
feature was dead end-to-end. Two lessons: (1) regenerate pico_view_bg.wasm
whenever the shared caco-picophony snapshot schema changes; (2) new snapshot
fields need an end-to-end LIVE assertion, not just a static guard. The bd-04d343
deterministic-scenario fix paid off immediately by enabling this catch.

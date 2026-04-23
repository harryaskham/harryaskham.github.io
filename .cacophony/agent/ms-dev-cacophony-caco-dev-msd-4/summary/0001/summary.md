# Session summary — bd-723688 status badge active-state mapping

## Goal
Stop rendering `? active` for healthy services in caco service status/show.

## Bead(s)
- `bd-723688` — service status `? active` glyph + bd-0e1bac `not_configured` generalisation

## Before state
- `StatusIcon::from_state` matched `running` but not `active`, so systemd's high-level state rendered with the Unknown glyph (`?`).
- `Token::from_state` had the same blind spot, so even if the icon were fixed the colour would still be muted.
- `not_configured` (failover) fell through to Unknown for the same reason.

## After state
- `active`, `loaded` → Ok (✓ green).
- `inactive` → Error; `activating`/`deactivating` → Warning (transitional).
- `not_configured`, `absent`, `none` → Absent (intentional non-presence).
- Token::from_state kept in lockstep so badge icon and badge text colour agree.
- 3 new tests pin the mappings, including an end-to-end smoke that `caco service status` no longer renders `? active`.

## Diff summary
- `crates/caco-cli/src/style.rs` (+88 / -8): expanded matchers + 3 tests.
- `cargo test-small`: 146 passing. caco-cli style tests: 32 passing.

## Operator-takeaway
Healthy systemd services now render `✓ active` everywhere status badges appear (caco service status/show, caco status, peer renderers). Failover `not_configured` now renders distinctly from genuine unknown-state failures.

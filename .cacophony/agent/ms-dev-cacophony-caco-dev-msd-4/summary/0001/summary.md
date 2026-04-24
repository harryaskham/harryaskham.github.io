# Session summary — bd-548e77 caco profile show JSON envelope

## Goal
Wrap `caco profile show --json` in the standard {ok, data, meta} envelope.

## Bead(s)
- `bd-548e77` — profile show JSON emits bare object; 9th distinct envelope shape

## Before state
- `caco profile show --json` returned bare profile object.
- Inconsistent with profile list, fleet snapshot, agent get, etc.

## After state
- New `wrap_profile_show_envelope` pure helper.
- `dispatch_profile_show` JSON branch wraps via helper; upstream meta propagates; missing meta becomes empty object.
- 2 new tests pin envelope shape + meta propagation.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+60 / -1)
- cargo test-small: 204 pass.

## Operator-takeaway
JSON consumers can now uniformly read `payload.data` across the profile namespace (and sister surfaces). Existing tooling reading the bare object will need to update to `payload.data`.

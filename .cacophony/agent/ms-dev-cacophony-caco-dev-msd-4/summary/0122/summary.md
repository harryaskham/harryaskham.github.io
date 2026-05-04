# bd-bf76a3 — Fix fullscreen button sizing and duplicate exit buttons

## Changes
- Kept the dashboard fullscreen toggle compact with explicit non-growing sizing (`flex: 0 0 auto`, `width: auto`, `max-width: max-content`, `white-space: nowrap`).
- Gave the mobile fullscreen control a fixed icon-button footprint (`34px`) so it cannot stretch across the top bar.
- Hid the desktop `.app-fullscreen-toggle` in the mobile viewport, leaving a single mobile fullscreen/exit control and preventing duplicate exit buttons.
- Added a caco-web static regression test covering compact sizing and single mobile viewport behavior for `bd-bf76a3`.

## Validation
- `cargo test -p caco-web fullscreen_toggle_stays_compact_and_single_per_mobile_viewport_bd_bf76a3 -- --test-threads=2`
- `cargo test -p caco-web no_bare_hex_outside_root_in_property_position -- --test-threads=2`
- `git diff --check`

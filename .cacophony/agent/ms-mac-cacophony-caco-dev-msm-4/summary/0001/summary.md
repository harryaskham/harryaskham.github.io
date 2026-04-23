# Session summary — bd-41a92d cluster-pulse modal teardown on navigation

## Goal

Fix a P0 operator-reported freeze: expanding the homepage cluster-pulse
animated graph into its fullscreen modal and then navigating away
(sidebar click, hashchange, hotkey, browser back/forward) left the modal
backdrop active and `body.overflow=hidden`. The previous graph instance
remained visible on top of the new view, the backdrop captured all
pointer events, and the operator had no in-page way out short of a full
reload. Make the modal robust against all four navigation paths and lock
the invariant in via tests so a future drive-by cannot regress it.

## Bead(s)

- `bd-41a92d` — Fix animated graph UI freezing on page navigation (P0)

## Before state

- Failing tests: none. Bug was operator-reported, not test-detected —
  the existing `app_js_has_cluster_pulse_module` test covered module
  shape but not lifecycle.
- `openClusterPulseModal()` mounted `clusterPulse` onto the modal canvas
  and set `body.overflow=hidden` but installed no Escape handler and no
  hashchange/popstate listeners.
- `switchView()` re-mounted the singleton onto the new view's canvas
  but never inspected or torn down a possibly-active modal.
- Result: navigating with the modal open left the modal "active" forever.

## After state

- Failing tests: none. `cargo test -p caco-web --lib` = 91/91 green;
  `cargo clippy -p caco-web --tests` clean.
- `switchView()` now removes the modal's `active` class and clears
  `body.overflow` before re-mounting onto the new view's canvas.
- `openClusterPulseModal()` installs (idempotently, guarded by
  `_clusterPulseModalListenersInstalled`) global keydown (Escape),
  hashchange, and popstate listeners that all converge on
  `closeClusterPulseModal()`.
- New test `app_js_cluster_pulse_modal_tears_down_on_navigation`
  enforces the four invariants: bd-41a92d guard comment, switchView
  classList.remove, Escape handler wired to close, hashchange +
  popstate listeners, and the listener-install flag.

## Diff summary

- Commits: `7f510298` (bd-41a92d).
- Files touched:
  - `crates/caco-web/static/app.js` (switchView teardown +
    openClusterPulseModal listener install)
  - `crates/caco-web/src/tests.rs` (+1 invariant test)
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: opening the fullscreen graph and pressing Escape
  now closes it. Navigating away with the modal open now reliably
  closes it instead of leaving a frozen overlay.

## Operator-takeaway

**Navigation-bound overlays in caco-web need four teardown paths**
(close button + Escape + hashchange/popstate + view-switch hook), not
one. Every existing ad-hoc overlay implements the close button and
relies on the operator finding it; bd-41a92d showed how easily the
other three get forgotten. I filed `bd-3fe180` as a draft to extract
a `registerOverlay({el, onClose})` helper so future overlays get the
contract for free instead of re-inventing it (and forgetting three
quarters of it).

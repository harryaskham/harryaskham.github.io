# Session summary — bd-4c2a1b cluster-pulse-expand-btn design-system alignment

## Goal

Replace the hard-coded rgba colours and bespoke transition declaration
on `.cluster-pulse-expand-btn` with the canonical design-system tokens
used by the rest of caco-web, so the homepage hero's expand affordance
no longer drifts visually from the surrounding UI.

## Bead(s)

- `bd-4c2a1b` — Style cluster-pulse-expand-btn to match web surface design (P2 task)

## Before state

- `.cluster-pulse-expand-btn` used:
  - `background: rgba(46, 52, 64, 0.12)` (hard-coded, not a token)
  - `color: rgba(229, 233, 240, 0.38)` (hard-coded)
  - `border: 1px solid transparent` (no token)
  - bespoke 6-line `transition: ...` block with literal cubic-bezier
  - hover/focus colours all hard-coded rgbas
- The rest of caco-web (.btn, .btn-icon, .btn-xs, .btn-primary) leans on
  `--bg-tertiary`, `--text-secondary`, `--border`, `--accent-soft`,
  `--accent`, `--border-glow`, `--shadow-sm`, and `var(--transition)`.
- Result: button was visually inconsistent (different hover ramp shape,
  different border treatment, no shadow on hover) with primary/secondary
  action buttons.

## After state

- `.cluster-pulse-expand-btn` now uses design-system tokens throughout:
  - base: `--bg-tertiary` / `--text-secondary` / `--border`
  - transition: `var(--transition)` (single source of truth)
  - hover: `--accent-soft` + `--border-glow` + `--accent` + `--shadow-sm`
    (mirrors `.btn:hover` ergonomics)
  - focus-visible: `--accent-soft` + `--border-glow`
- The recede-into-corner behaviour (opacity 0.38 → 0.72 on hero hover →
  1.0 on direct hover) is preserved exactly. Sizes (18×18) preserved.
- Tests: 258/258 caco-web lib tests pass (cluster_pulse 6/6).

## Diff summary

- Commits: 11b5190ea (cherry-picked through agent-branch FF recovery)
- Files touched:
  - `crates/caco-web/static/style.css` — refactored
    `.cluster-pulse-expand-btn` rule and its `:hover` / `:focus-visible`
    states to use design-system tokens
  - `crates/caco-web/src/tests.rs` — relaxed the cubic-bezier assertion
    to accept the design-token form, and added a new test
    `cluster_pulse_expand_btn_uses_design_tokens_bd_4c2a1b` pinning
    the token contract
- Tests: +1 (token contract regression guard) / -0 / 0 flipped
- Behavioural delta: expand button hover/focus now visually matches the
  rest of the web surface — same `--accent-soft` background, same
  `--border-glow` border, same `--shadow-sm` lift. No functional change
  to the modal expand action.

## Operator-takeaway

Per the workspace DO-OVER directive (bd-5bfb2c) the operator wants
caco-web surfaces using shared canonical components rather than
forking. This bead is the small-button version of that principle:
audit raw rgba literals on individual UI elements and fold them back
to the design-system tokens. The new
`cluster_pulse_expand_btn_uses_design_tokens_bd_4c2a1b` test makes the
contract self-policing for future refactors.

Side artefact: filed bd-fab8ff (closed as duplicate of operator's
bd-de3fa6) when reintegrate-time smoke surfaced
`docs_html_pages_load_webapp_fonts_and_favicon` failing on main —
beelink (technical writer) is restoring the docs font preconnect as
a docs-only hotfix.

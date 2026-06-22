# Session summary — bd-873a66: prefers-reduced-transparency: reduce

## Goal

Continue a11y feature parity. Codebase honoured 3 sibling
accessibility preferences but missed the 4th (transparency).

## Bead(s)

- `bd-873a66` — [caco-web] support prefers-reduced-transparency: reduce

## Existing a11y preference family

| Preference | Coverage |
|---|---|
| `prefers-reduced-motion` | 32 declarations across stylesheets |
| `prefers-contrast: more` | workspace-a11y.css:222 (borders/outlines/active tab) |
| `:focus-visible` | real-keyboard-focus only (multiple places) |
| `prefers-reduced-transparency` | **MISSING** (now added) |

## Why this matters

The dashboard uses `backdrop-filter: blur(...)` on
**66+ surfaces** in `style.css` alone (modal, toast,
panel, stat-card, node-card, filter-chip-row,
batch-action-bar, sticky table headers,
dialog::backdrop, chat-slash-suggest, mobile-topbar,
cluster-pulse-expanded-backdrop, etc.).

Users who enable macOS "Reduce Transparency" (System
Settings → Accessibility → Display) or Windows
equivalent find heavy backdrop-blur:

- **Visually overwhelming** — blurred backdrop competes
  with foreground text.
- **Legibility-degrading** — translucent blurred
  backgrounds are harder to read than solid ones.
- **Cognitive-load-increasing** — blur creates
  ambiguity about where one surface ends and another
  begins.

OS exposes this via the `prefers-reduced-transparency`
media query (Chrome 118+, Safari 17.4+, Firefox 113+).

## Fix

Added a new `@media (prefers-reduced-transparency:
reduce)` block to `workspace-a11y.css` (the established
a11y-overrides surface, sibling to the existing
`prefers-contrast: more` block) that:

1. **Universally strips backdrop-filter** — `*, *::before, *::after { backdrop-filter: none !important; -webkit-backdrop-filter: none !important; }`.
   The universal selector is appropriate for one-off a11y
   override blocks; same approach the 32 reduced-motion
   overrides use across stylesheets.
2. **Flattens 10 common glass surfaces** to opaque
   `var(--bg-secondary)`: `.modal-content`, `.toast`,
   `.panel`, `.stat-card`, `.node-card`,
   `.filter-chip-row`, `.batch-action-bar`,
   `.chat-slash-suggest`, `.mobile-topbar`, `.toolbar`.

Browsers without support fall through to existing glass.
Zero visible change for users without the preference.

## Test design (6 layers)

1. Brace-depth block scoping isolates the @media body.
2. Universal-strip declarations checked for BOTH
   standard and -webkit-prefixed.
3. Selector chain covers `*::before` and `*::after` for
   glass overlay pseudos.
4. 5 common surface classes pinned for opaque flatten.
5. Sibling-presence pin for `@media (prefers-contrast: more)`.
6. **Cross-stylesheet family count layer** — iterates
   13 stylesheets and asserts >= 30
   `prefers-reduced-motion` declarations remain, so a
   broader a11y-preference-family regression would fail.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/workspace-a11y.css` -- new ~50-line @media block with 27-line comment header documenting the rationale and browser support.
  - `crates/caco-web/src/tests.rs` -- regression test with 6 assertion layers.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 486 -> 487; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Operators running macOS "Reduce Transparency" or
Windows equivalent now see a flat, opaque, high-
contrast dashboard with the glass blur fully stripped.
The dashboard's accessibility-preference family is now
complete across the 4 main user-visible OS hints:
motion, contrast, focus, and transparency.

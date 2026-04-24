# Session summary — quick-bead buttons stop truncating; stale-check docs row restored

## Goal

Land a tiny web/docs cleanup bundle that removed friction from the
quick-bead modal and cleared a mainline docs drift failure at the same
time: keep both quick-bead action buttons from being squashed by the
project selector, and add the missing `stale-check` profile row to
`docs/profiles.html` so the shipped-profile catalog test goes green.

## Bead(s)

- `bd-8a38e2` — Fix `Expand with AI` button truncation and misalignment
- `bd-3370cf` — Fix `File as-is` button truncation and misalignment
- `bd-eccbbc` — `[broken-on-main] shipped_profiles_html_lists_every_canonical_profile` failing — `stale-check` row missing from `docs/profiles.html`

## Before state

- Failing tests: `caco-profile::tests::shipped_profiles_html_lists_every_canonical_profile`
  panicked because `stale-check.md` existed in `.cacophony/profiles/`
  but the matching `<tr>` row was absent from `docs/profiles.html`.
- Quick-bead modal action row used an inline flex container where the
  project `<select>` and both buttons competed for width. The buttons
  were allowed to shrink, so long labels (`Expand with AI`, `File as-is`)
  got truncated / visually misaligned.
- `cargo test-small` could not complete until the broken-on-main docs
  drift was fixed.

## After state

- `docs/profiles.html` now includes a `stale-check` row, matching the
  checked-in canonical profile inventory.
- Quick-bead buttons no longer shrink under flex pressure:
  - `#quick-bead-expand-btn` and `#quick-bead-direct-btn` now use
    `flex-shrink: 0`
  - `#quick-bead-project` absorbs the squeeze via `flex: 1 1 auto`
    plus `min-width: 0`
- `cargo test -p caco-profile --lib shipped_profiles_html` passes again.
- `cargo test-small` passes cleanly: 182 / 182 green.

## Diff summary

- Files touched:
  - `crates/caco-web/static/style.css`
  - `docs/profiles.html`
- Tests run:
  - `cargo test -p caco-profile --lib shipped_profiles_html`
  - `cargo test-small`
- Behavioural delta:
  - quick-bead modal action row keeps full button labels visible
  - shipped profile docs stay aligned with `.cacophony/profiles/`

## Operator-takeaway

This was a good example of two tiny, real annoyances sharing one clean
landing window: a broken-on-main docs drift and a visible webapp paper-cut.
The CSS fix is intentionally minimal and local, and the docs row keeps the
profile inventory self-check doing useful work instead of becoming background
noise.

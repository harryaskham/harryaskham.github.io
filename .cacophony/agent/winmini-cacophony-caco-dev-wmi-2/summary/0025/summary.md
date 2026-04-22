# bd-bf1e86 polish #12: hooks empty state gets keystroke hint parity

## Goal

Same surgical empty-state polish as #11, applied to `crates/caco-tui/src/views/hooks.rs::render_empty`. The Hooks view's empty state was a near-twin of Crons' (same structure, different label/key) and needed the same `Press \`r\` to refresh, \`?\` for help.` hint line for parity.

## Bead(s)

- bd-bf1e86 (permanent polish track) — cycle #12

## Before state

The `render_empty` for the Hooks view rendered "No hooks configured" + a one-liner about cacophony.yaml — no keystroke hint, unlike beads/notifications/crons.

## After state

Appended one styled hint line (`Press \`r\` to refresh, \`?\` for help.`, nord::NORD3 subdued style). Existing `render_empty_shows_no_hooks` test extended to assert the new hint is present.

## Diff summary

1 file changed, +14 / −0:

- `crates/caco-tui/src/views/hooks.rs`:
  - +9 lines: blank line + styled keystroke hint Span + bd-bf1e86 polish #12 comment
  - +5 lines: extended `render_empty_shows_no_hooks` test assertion for the new hint

## Verification

- `cargo test -p caco-tui --lib views::hooks`: 5/5 PASS
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Operator-takeaway

Same low-risk pattern as polish #11 (crons). Empty-state-keystroke-hint parity now consistent across: beads, notifications, crons, hooks. Next candidate views to audit for the same gap: profiles, builds, releases, tests.

bd-bf1e86 cycle counter: 12/session.

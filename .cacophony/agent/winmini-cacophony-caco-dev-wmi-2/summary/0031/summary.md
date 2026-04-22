# bd-bf1e86 polish #17: errors empty state gets keystroke hint parity

## Goal

Bring the per-project Errors view's empty state to keystroke-hint parity with crons/hooks/profiles (polish #11/#12/#13).

## Bead(s)

- bd-bf1e86 (permanent polish track) — cycle #17

## Before state

`crates/caco-tui/src/views/errors.rs::render_empty` showed the standard "No exceptions" message + project description but no `r`/`?` keystroke hint.

## After state

Appended one styled hint line `Press \`r\` to refresh, \`?\` for help.` (nord::NORD3), matching the established pattern.

Verification:
- `cargo build -p caco-tui`: clean
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

1 file changed, +6 / −0:

- `crates/caco-tui/src/views/errors.rs::render_empty`: 6 lines (blank + hint + bd-bf1e86 polish #17 comment)

## Operator-takeaway

Errors view is moderate-traffic (operators check it after build/test failures). With this cycle the empty-state-keystroke-hint pattern now covers: beads (per-project + global), notifications, crons, hooks, profiles, fuzzy_picker, chat, errors. Builds/releases/tests/actions all have domain-specific hints already. Workspace_picker has Esc/Enter.

bd-bf1e86 cycle counter: 17/session. The pattern is essentially saturated; remaining empty-state files (diff_view, mode_selector, console, configuration) are interactive forms or read-only viewers where the existing affordances are appropriate.

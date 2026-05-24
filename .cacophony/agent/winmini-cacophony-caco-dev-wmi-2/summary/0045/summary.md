# Session summary — CLI Kitty inline graphics no longer move the cursor

## Goal

Fix correctness risks in the CLI-only Kitty inline graphics helper so decorative bitmap enhancements behave like true background adornments: they must not move the terminal cursor, must not leak Kitty response payloads into command output, and must size placements using terminal display width for Unicode text.

## Bead(s)

- `bd-6cdf67` — Fix CLI Kitty inline graphics cursor and response semantics

## Before state

- `emit_kitty_inline` emitted `a=T,f=100,z=-1,c=<cols>,r=<rows>,m=<...>` without `C=1`, so Kitty placement could advance the cursor.
- Decorated text call sites compensated with `CSI <cols>D` cursor-left sequences before printing text.
- CLI graphics uploads did not request quiet mode even though normal CLI commands do not read Kitty graphics responses.
- The CLI width helper used `chars().count()`, so wide CJK text and combining marks could produce incorrect `c=` placement widths.

## After state

- CLI inline Kitty APC uploads now include `C=1` and `q=2` on the first transmit/display chunk.
- Glow, box, and pill decorations no longer emit cursor-left compensation sequences; text is printed after a non-moving bitmap placement.
- Box and pill decorations explicitly print their left/right padding as text cells around the decorated content.
- `caco-cli` now uses the workspace `unicode-width` dependency for terminal display-cell sizing, and `caco-tui` was switched to the shared workspace dependency entry.
- `SPEC.md` now records the CLI inline graphics requirement: non-moving placement, quiet mode, and display-width sizing.

## Diff summary

- Code/content commits: `8e1e03db3` (`bd-6cdf67: fix CLI kitty inline placement semantics`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched:
  - `Cargo.toml`
  - `Cargo.lock`
  - `SPEC.md`
  - `crates/caco-cli/Cargo.toml`
  - `crates/caco-cli/src/style.rs`
  - `crates/caco-tui/Cargo.toml`
- Tests: +3 focused unit tests / -0 / flipped 1 existing width test into display-width coverage
- Validation:
  - `git diff --check`
  - queued `cargo test -p caco-cli bd_6cdf67 -- --nocapture` via `caco test run`, passed as `tj-415a66df`
- Behavioural delta: CLI bitmap decorations should remain underneath the same text cells without cursor backtracking, suppress unsolicited Kitty responses, and reserve enough cells for wide Unicode while not over-counting combining marks.

## Operator-takeaway

The fix removes the fragile assumption that Kitty image placement advances horizontally only. CLI graphics now ask Kitty not to move the cursor at all, so the text stream stays simpler and less likely to corrupt command output in real terminals or shells.

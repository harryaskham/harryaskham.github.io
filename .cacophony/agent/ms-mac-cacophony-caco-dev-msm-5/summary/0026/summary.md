# Session summary — TUI summary artefact labels

## Goal

Continue TUI summaries polish by making embedded artefact rows more informative and closer in clarity to the web and Android artefact treatments.

## Bead(s)

- `bd-20feaa` — TUI summaries: improve embedded artefact detail labels
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The TUI detail pane listed sibling artefact filenames, but labels were terse: `terminal.cast`, `data.json`, and screenshot names.
- Operators could see that files existed but not what each artefact type was for.
- Large screenshot bundles could crowd the terminal detail pane with every filename.

## After state

- Artefact rendering is factored into `push_artefact_affordances`.
- The artefact section now shows a total file count and notes that raw files are served through the summaries API for web/Android viewing.
- `terminal.cast` is labelled as an asciinema terminal recording.
- `data.json` is labelled as structured metrics / machine-readable data.
- Screenshot bundles show count, purpose, up to four readable filenames, and a compact remaining-count line.

## Diff summary

- Commits: current `bd-20feaa` implementation commit
- Files touched:
  - `crates/caco-tui/src/views/summaries.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: no API change; the terminal detail pane now communicates artefact meaning and avoids overwhelming long screenshot lists.

## Operator-takeaway

TUI summaries now explain embedded artefacts instead of just dumping filenames, making terminal-only review closer to the richer web and Android experience.

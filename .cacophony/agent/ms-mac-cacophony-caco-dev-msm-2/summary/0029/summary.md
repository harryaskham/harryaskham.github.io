# Session summary — caco-tui persistent profile

## Goal

Encode the Tendril-driven Cacophony TUI improvement workflow into a persistent profile so ms-mac can run this loop deliberately instead of relying on ad hoc operator memory. The profile needed to be endless, focused on assigned TUI work only, capture visual evidence into recorded summaries, and document PR-backed reintegration with direct fallback.

## Bead(s)

- `bd-73ddbe` — Encode caco-tui persistent profile (`.cacophony/profiles/caco-tui.md`) — TUI improvement via tendril

## Before state

- Failing tests: no profile-specific test failures known. Concurrent broken-on-main issues were already owned by other agents: clippy `open_or_update_pr` too_many_arguments by helsinki, and caco-web docs font/favicon by pocket4.
- Relevant metrics: `.cacophony/profiles/caco-tui.md` did not exist.
- Context: the Tendril visual-QA loop existed only as operator/controller instruction and recent bead history, so a persistent agent could not reliably reproduce the full workflow or avoid accidental general auto-claim behaviour.

## After state

- Failing tests: none observed for this profile-only change.
- Relevant metrics: `caco config validate` passed.
- Context: `.cacophony/profiles/caco-tui.md` now defines a persistent endless worker profile with Cacophony MCP servers, Tendril Pi config wiring, no-autoclaim operating rules, screenshot capture paths under `.cacophony/agent/<agent-id>/summary/<index>/screenshots/`, ms-mac constraints, Tendril launch guidance via `nix run ${CACOPHONY_DIR}/daemon/checkouts/tendril`, and PR-review plus direct fallback reintegration guidance. A first direct reintegration attempt incorrectly reported a redundant merge while the file was absent from `origin/main`; the work was recovered from reflog and resubmitted.

## Diff summary

- Commits: `69af87540`
- Files touched: `.cacophony/profiles/caco-tui.md`
- Tests: profile/config validation only; no Rust tests added.
- Behavioural delta: a new profile can now be selected for a persistent caco-tui worker. It cross-links `bd-c04159`, `bd-eefb8d`, and `bd-f85b12`, encodes the capture-act-verify Tendril workflow, and explicitly forbids draining the general queue.

## Operator-takeaway

The caco-tui loop is now a first-class repo profile rather than tribal knowledge: future persistent TUI workers should know how to launch Tendril, capture evidence, file reproducible UI bugs, validate focused fixes, and reintegrate without accidentally becoming general-purpose autoclaim workers.

# Session summary — summaries now read from `cacophony-state`

## Goal

Fix the summaries viewers so TUI, web, and Android read the recorded summary
artefacts from the canonical `cacophony-state` branch rather than whatever
happens to be present in the checkout working tree. The goal was a single
backend fix at the daemon summary API layer so all three clients would follow
without client-specific branching logic.

## Bead(s)

- `bd-fddde3` — `[summaries] TUI/web/android load summaries from wrong branch instead of cacophony-state`

## Before state

- TUI, web, and Android all consume the daemon’s `/api/v1/summaries` API family.
- The daemon summary endpoints were reading directly from:
  - `<checkout>/.cacophony/agent/<agent>/summary/<index>/summary.md`
  - sibling artefacts under the checkout filesystem
- That means the viewers reflected the checkout tree, not the committed
  `cacophony-state` artefact branch.
- If `main` (or any working tree state) drifted from `cacophony-state`, the
  viewers could show the wrong summary content/artefacts.

## After state

- The daemon summary readers now resolve the project state branch and read
  summaries/artefacts from git on that branch instead of the checkout tree.
- Added branch-backed readers in `crates/caco-daemon/src/summary.rs` for:
  - list enumeration
  - parsed summary show
  - raw sibling artefact fetch
- `/api/v1/summaries`, `/api/v1/summaries/{agent}/{index}`, and the raw
  summary artefact route now use the state-branch-backed readers.
- The implementation honours per-project state-branch overrides via
  `ProjectBranchesConfig::resolve_cacophony_state(...)` instead of hardcoding
  only the default branch name.
- TUI, web, and Android all benefit automatically because they already consume
  the daemon summary API.
- Live probe still works after the change:
  - `cargo run -q -p caco -- summaries list --project cacophony --limit 3 --json`

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/summary.rs`
  - `crates/caco-daemon/src/lib.rs`
- Validation:
  - `cargo build -p caco-daemon`
  - targeted: `cargo test -p caco-daemon state_branch_reader_prefers_cacophony_state_over_checkout_tree -- --nocapture`
  - `cargo test-small`
  - live sanity: `cargo run -q -p caco -- summaries list --project cacophony --limit 3 --json`
- Regression coverage added:
  - `state_branch_reader_prefers_cacophony_state_over_checkout_tree`
  - proves that if `main` and `cacophony-state` disagree, the summary reader
    prefers the `cacophony-state` version
- Behavioural delta:
  - summary viewers now consistently show the committed artefact branch view,
    not incidental checkout content

## Operator-takeaway

The bug was centralized, not client-specific: the daemon summary API was
walking the checkout filesystem. Fixing the daemon to read from
`cacophony-state` corrects TUI, web, and Android together and aligns the
viewer contract with how recorded session artefacts are actually published.

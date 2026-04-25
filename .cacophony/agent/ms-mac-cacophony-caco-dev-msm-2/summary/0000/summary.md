# Session summary — mirror permanent auto-claim fallback filters

## Goal

Close a queue-hygiene gap found during collab-mode: the daemon now skips permanent trackers by status, title prefix, or label, but the CLI's defensive stale-daemon fallback only recognized permanent status.

## Bead(s)

- `bd-6af77f` — [bug] CLI permanent auto-claim fallback misses title and label trackers

## Before state

- Failing tests: no regression covered title-prefix or label-based permanent tracker detection in `auto_claimed_permanent_bead_message`.
- Relevant metrics: ready queue was empty, and beadless claim was already skipping status-permanent trackers cleanly after bd-45cdba.
- Context: if a stale/routed daemon returned a tracker represented as `[PERMANENT]` title prefix or `permanent` label while status was `in_progress`, the CLI fallback could report a successful implementation claim.

## After state

- Failing tests: none in targeted or small validation.
- Relevant metrics: `timeout 120 cargo test -p caco-cli --lib auto_claimed_permanent_bead_message_detects_title_and_label_trackers_bd_6af77f -- --nocapture` passed; `timeout 180 cargo test-small` passed.
- Context: the fallback now mirrors the daemon guard for status permanent, trim-start `[PERMANENT]` title prefix, and case-insensitive `permanent` labels.

## Diff summary

- Commits: 43e629c4a
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added `auto_claimed_permanent_bead_message_detects_title_and_label_trackers_bd_6af77f`.
- Behavioural delta: stale or routed daemon responses are rejected consistently across all three tracker encodings, preventing workers from being stranded on permanent tracker beads.

## Operator-takeaway

The CLI-side safety net now matches the daemon's permanent-tracker semantics, so queue drain remains safe even during mixed-version or routed-beads windows.

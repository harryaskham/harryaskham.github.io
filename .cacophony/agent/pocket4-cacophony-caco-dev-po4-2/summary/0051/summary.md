# Session summary — scope lineage filter test assertion

## Goal

Fix `bd-bc1c85`, a broken-on-main caco-cli test where lineage filter validation asserted against the entire rendered status output even though that output intentionally includes both an unfiltered lineage-chain summary and a filtered query section.

## Bead(s)

- `bd-bc1c85` — [broken-on-main] caco-cli lineage filter test asserts against unfiltered summary

## Before state

- Failing tests: queued `cargo test -p caco-cli lineage -- --test-threads=1` had failed on `tmux_send_mutex_tests::agent_status_lineage_options_render_filters_and_query_bd_a936c0`.
- Relevant metrics: the failing assertion checked `!rendered.contains("agent-other at")`, but `agent-other` legitimately appears in the unfiltered lineage-chain summary before the filtered query results.
- Context: the filtered query output itself was correct; the test scope was too broad.

## After state

- Failing tests: none in the targeted lineage test lane.
- Relevant metrics: the test now splits the rendered output at the filtered query header, asserts the matching `agent-new` record is present inside that query section, and asserts the non-matching `agent-other` record is absent only from that scoped section.
- Context: unfiltered lineage-chain summary behavior remains intact while the query filter expectation is precise.

## Diff summary

- Code/content commits: `00af1d516`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +0 / -0 / flipped 1 assertion scope
- Behavioural delta: no production behavior change; this is a test correction that preserves the intended unfiltered summary plus filtered query rendering contract.
- Validation: queued `cargo test -p caco-cli lineage -- --test-threads=1` passed as `tj-bb511645`.

## Operator-takeaway

The lineage rendering was not broken; the test was. It now validates the filtered query section without rejecting the intentional unfiltered lineage-chain summary.

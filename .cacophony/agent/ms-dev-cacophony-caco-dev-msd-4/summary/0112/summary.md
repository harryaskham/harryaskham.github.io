# Session summary — JSON validator envelopes for summaries and event log

## Goal

Fix CLI-side validator failures that leaked human stderr instead of machine-readable JSON when `--json` was requested. The original bead covered `caco summaries show`; a direct message addendum asked for the same fix on `caco event log` validators.

## Bead(s)

- `bd-f31814` — `caco summaries show --json validator errors emit human stderr rc=2 with no JSON envelope`

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: Repros included `caco summaries show --index abc --json` and `caco event log --since abc --json` returning human error text with no `{ok:false,error:{...}}` envelope.
- Context: Sister fixes already existed for scratch and timeline validators. This bead covered another pair of CLI-side validation paths that run before daemon calls.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `summaries show` now returns `{"ok":false,"error":{"code":"invalid_argument"...}}` with exit 1 for invalid `--index`/empty `--agent`/invalid project in JSON mode, while preserving human errors in text mode. `event log` now wraps invalid `--since` and invalid `--limit` as JSON invalid-argument envelopes before reaching the daemon.
- Context: Event log still performs its internal validation as a belt-and-braces check; the dispatcher now handles the JSON envelope path before calling it.

## Diff summary

- Commits: implementation commit `bd-f31814: emit JSON validator errors for summaries and events` plus the summary-only commit for this record.
- Files touched: `crates/caco-cli/src/summary_cmd.rs`, `crates/caco-cli/src/lib.rs`.
- Tests: added `summaries_show_invalid_index_json_envelope_bd_f31814`, `event_log_invalid_since_json_envelope_bd_f31814`, and `event_log_invalid_limit_json_envelope_bd_f31814`.
- Behavioural delta: invalid summaries/event-log arguments under `--json` now return canonical JSON error envelopes with exit 1 instead of raw human stderr/rc=2.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-cli bd_f31814 -- --nocapture`.

## Operator-takeaway

Machine consumers can now parse validator failures from both `caco summaries show --json` and `caco event log --json`, including the addendum cases for bad `--since` and `--limit`, without special-casing raw stderr.

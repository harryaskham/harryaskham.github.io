# Session summary — bd-1b0adb gold-standard daemon-unreachable wording

## Goal

Stop high-frequency caco surfaces (`caco bd show`,
`caco msg inbox`, `caco changelog show`, ...) from emitting a
sparse `daemon request failed: <details>` on transient daemon-down
when sister `caco event log` already had the operator-actionable
`failed to reach daemon — is it running? (try: caco up)
[<details>]` wording.

## Bead(s)

- `bd-1b0adb` — caco changelog show / msg inbox / bd show emit sparse error vs gold-standard caco event log (P4 bug, test-user pass)

## Before state

Three drifts joined to produce the inconsistency:

1. Shared `transport_cli_error` text-mode helper fell back to
   `daemon request failed: <e>` when no sidecar lifecycle envelope
   was available.
2. Sister `transport_error_envelope` JSON helper did the same in
   the `error.message` field.
3. ~20 inline call sites in `crates/caco-cli/src/lib.rs` and
   `audio_cmd.rs` hand-rolled
   `CliError::new(format!("daemon request failed: {e}"))` instead
   of routing through the helper, so any change to the helper
   couldn't reach them.

## After state

- `transport_cli_error` fallback emits the gold-standard
  `failed to reach daemon — is it running? (try: caco up) [<e>]`
  with the operator-actionable hint.
- `transport_error_envelope` mirrors the wording for `--json`
  consumers.
- ~20 inline `CliError::new(format!("daemon request failed: {e}"))`
  call sites in `lib.rs` + `audio_cmd.rs` mass-converted to
  `transport_cli_error(&e)` via `sed`. Centralisation gives them
  the bd-88ddd7 sidecar lifecycle fallback for free.
- The `scrub_daemon_url_from_transport_error` path (bogus
  `--field-get` value) now seeds with the gold-standard wording
  too, so URL-scrub errors aren't sparse either.
- `looks_transport` heuristic in the restart-window retry loop
  broadened to recognise both `daemon request failed` and
  `failed to reach daemon`, so transient-daemon-down still
  triggers the retry path.
- Existing `bd_send_request` transport-failure test broadened to
  accept the new fourth valid phrasing alongside the three
  pre-existing ones.

## Diff summary

- Commit: ed8991f8f
- Files touched:
  - `crates/caco-cli/src/lib.rs` (+85, -28) — helper rewrites,
    inline-callsite mass conversion, retry-loop detection,
    test-loosen, new source-grep test.
  - `crates/caco-cli/src/audio_cmd.rs` (+1, -1) — single inline
    callsite converted.
- New `transport_cli_error_uses_gold_standard_wording_bd_1b0adb`
  greps the helper + JSON envelope bodies and asserts:
  - both contain `failed to reach daemon`,
  - both contain the `(try: caco up)` hint,
  - the sparse legacy `daemon request failed: {e}` is gone from
    `transport_cli_error`.
- Tests: cargo test-small 264/264 pass.

## Operator-takeaway

Any caco verb that talks to the daemon now produces the same
gold-standard wording on transient daemon-down: the operator sees
`is it running? (try: caco up)` and knows the corrective action.
The centralised helper makes this drift-resistant — future caco
subcommands that route through `transport_cli_error` (or via
`bd_send_request` / `async_send_request`) inherit the wording for
free, and the new test pins it in place.

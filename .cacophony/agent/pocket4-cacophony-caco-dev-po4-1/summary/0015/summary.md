# Session summary — bd-cb3ca8 reject placeholder claim payloads

## Goal

Stop `caco bd claim` from printing the misleading
`claimed: ? — ? (assignee: ?)` success line when the daemon enters
a restart window mid-request and returns an `ok=true` response with
empty/missing `data`. Replace the placeholder line with an explicit
indeterminate-state error that points the caller at
`caco bd show --bead-id <id>` for confirmation.

## Bead(s)

- `bd-cb3ca8` — caco bd claim can print placeholder success during daemon restart (P3 bug)

## Before state

- `dispatch_bd_claim` and the create-and-claim format closures both
  extracted `id`, `title`, `assignee` via
  `body["data"][field].as_str().unwrap_or("?")` and unconditionally
  returned `Ok(format!(...))`.
- During a daemon-restart window (`bd-73ddbe` repro) the response
  shape carried `ok=true` but no populated `data`, so the rendered
  human line was `claimed: ? — ? (assignee: ?)` and the CLI returned
  exit 0. A follow-up `caco bd show` confirmed the bead remained
  open — the success line was a lie.

## After state

- Both format closures now extract the three fields as
  `Option<&str>` and gate the success format on
  `(Some(_), Some(_), Some(_))`.
- Missing fields surface as a `CliError` with the actionable text:
  `daemon returned ok=true but no claim payload (likely a
  restart-window response). Re-run 'caco bd show --bead-id <id>' to
  confirm actual state before retrying.`
- The error flows through `bd_daemon_result`, so both text and
  `--json` callers see a non-zero exit code; structured JSON
  consumers get the canonical error envelope.

## Diff summary

- Commit: 489e33ea5
- Files touched: `crates/caco-cli/src/lib.rs` (+84, -10)
- New source-level guard test
  `bd_claim_format_rejects_placeholder_payload_bd_cb3ca8` asserts
  (a) no `unwrap_or("?")` fallbacks remain in the closure windows
  around either format string and (b) the indeterminate-state
  error text is present in both windows.
- Tests: cargo test-small 261/261 pass; new test passes.
- Behaviour delta: only daemon-restart-window placeholder responses
  now error; happy-path claims with full payloads are unchanged.

## Operator-takeaway

`caco bd claim` is now all-or-nothing under daemon restart races:
either it returns a populated success line (real claim) or it
returns an explicit error pointing at the verification command.
This fits the bd-d47485 / bd-88ddd7 family of "give CLIs and agent
supervisors structured information about transport vs application
errors so they can decide whether to verify before retrying"
improvements — claim now contributes to that contract instead of
silently asserting success.

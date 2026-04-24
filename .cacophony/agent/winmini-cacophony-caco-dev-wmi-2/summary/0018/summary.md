# Session summary — bd-b724cb (Issue 4): caco profile show --name X --json structured failure envelope

## Goal

Address bd-b724cb's actionable item, Issue 4:
`caco profile show --name bogus --json` returned a
text error and exited 1 (4th JSON-broken-on-error
surface in the bd-241b84 catalog). Programmatic
consumers parsing `--json` get invalid output.

The bead's other contents are positive observations
(Issue 1 outbox-list structured-failure envelope already
landed via daemon; Issue 2 error-as-card promote; Issue
3 profile show 6th-phrasing inline-allowed-values; Issue
5 caco loop list/cancel CLI affordance is a separate
feature bead).

## Bead(s)

- `bd-b724cb` — `caco profile + outbox + loop + ls — outbox
  list NEW BEST-IN-CACO STRUCTURED FAILURE ENVELOPE...
  profile show --name bogus --json BROKEN exit 1 4th
  JSON-ignored-on-error...`.

## Before state

```
$ caco profile show --name bogus --json
error: profile 'bogus' not found. Available: ambient-mode,
  auto-claim, ... [54 names]
$ echo $?
1
```

Output is plain text on stderr; exit 1; `--json` ignored.
A consumer doing `caco profile show --name X --json |
jq 'select(.ok == false)'` would see jq parse failure
on the empty stdin (text went to stderr).

## After state

```
$ caco profile show --name bogus --json
{
  "ok": false,
  "error": {
    "code": "not_found",
    "message": "profile 'bogus' not found. Available: ambient-mode,
      auto-claim, ... [54 names]"
  }
}
$ echo $?
1
```

Structured failure envelope on stdout (parseable JSON);
exit 1 preserved (mirrors bd-d761db Issue 5 pattern of
inspecting `.ok` to set exit_code). Joins the bd-b724cb
{ok:false, error:{code, message}} family.

Note: `meta.request_id` deliberately omitted for now —
the local `cli_error_json` helper doesn't capture
reqwest response headers. Adding request_id propagation
is a cluster-wide refactor (every `cli_error_json`
caller would benefit) and belongs in its own bead.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `dispatch_profile_show`: replaced the `ok_or_else` →
    `CliError` not-found path with an explicit `match`
    that emits `cli_error_json("not_found", &msg)` when
    `json_requested`, returning the envelope as the
    function's `Ok(String)` result. Text path unchanged
    (still emits `Err(CliError::new(msg))` and exits 1).
  - Dispatcher arm `[cmd, sub] if cmd == "profile" && sub
    == "show"`: mirrors the bd-d761db Issue 5 pattern —
    inspect the returned envelope's `.ok` field to set
    `exit_code` (1 on `ok:false`, 0 on `ok:true`),
    return `Outcome` directly with paginate=true.
  - 1 new test:
    `dispatch_profile_show_emits_structured_error_envelope_on_json_not_found`
    — source-greps both the dispatcher arm marker
    (`bd-b724cb (Issue 4): profile show --json`) and the
    `dispatch_profile_show` body for `cli_error_json("not_found"`
    so the wrap can't silently regress.
- `cargo test -p caco-cli --lib dispatch_profile_show_...`:
  pass.
- `cargo test-small`: 182 pass.

## Operator-takeaway

The JSON-broken-on-error catalog drops by one. Of the 4
surfaces called out:
- bd-241b84 audio transcribe: still pending
- bd-f4957c node show --name: still pending
- bd-140660 snapshot pin: still pending
- profile show: shipped here

Each follows the same micro-pattern: inside the dispatch
function, branch on `json_requested` at the not-found
site; emit `cli_error_json(<code>, <message>)`; in the
dispatcher arm, parse `.ok` and set exit_code. Worth
consolidating into a shared helper (e.g.
`json_or_err(json_requested, code, msg)` that returns
the right `String`/`CliError` based on the flag) so
the next 3 retrofits are one-liners.

The cluster-wide gap — `meta.request_id` propagation —
is the next follow-up: today only daemon-originated
failures (HTTP 401/403/etc.) carry request_id because
the daemon emits it directly; client-side validators
that emit through `cli_error_json` lose the tracing
handle. A shared helper threading the X-Request-Id
header into cli_error_json would close the loop.

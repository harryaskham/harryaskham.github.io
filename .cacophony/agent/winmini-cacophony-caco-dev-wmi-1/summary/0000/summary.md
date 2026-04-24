# Session summary — bd-87425e: caco image generate --json honors errors

## Goal

Make `caco image generate --json` emit a structured
`{ok:false, error:{code,message}}` envelope on stdout for every
error path, instead of plain text on stderr with empty stdout.
Matches the gold-standard `caco bd show --json` error contract.

## Bead(s)

- `bd-87425e` — caco image generate --json IGNORES the --json flag
  on ALL error paths (P3 bug). Issue 1 (JSON error envelope) fixed
  here; Issue 2 (empty `--prompt` validator + HTTP-path leak) left
  for a follow-up bead if operator wants it pinned separately.

## Before state

```
$ caco image generate --project cacophony --preset bogus --json
[stdout: empty]
[stderr: error: unknown image preset 'bogus' — available: …]
[exit: 2]
```

Same pattern for `--project bogus`, empty `--prompt`, transport
errors. Script consumer doing
`if ! caco image generate … --json | jq -e .ok` got `null` /
"parse error" instead of a useful error envelope.

## After state

```
$ caco image generate --project cacophony --preset bogus --json
{
  "ok": false,
  "error": {
    "code": "image_generate_failed",
    "message": "unknown image preset 'bogus' — available: …"
  }
}
[exit: 1]
```

Wrapping at the dispatch boundary (rather than rewriting the
function's many `CliError::new(…)` sites) keeps the diff tight: a
single `match` around `dispatch_image_generate(...)` catches every
`Err` and converts it to a structured `Outcome` when `--json` is
in effect. Text-mode behaviour is unchanged.

## Diff summary

- 1 file changed, +18 / -2 (`crates/caco-cli/src/lib.rs` — dispatch
  branch only; the `dispatch_image_generate` body is untouched).

## Validation

- `cargo check -p caco-cli --tests`: clean.

## Operator-takeaway

`caco image generate … --json` now behaves like every other
JSON-aware caco surface: errors land as a parseable envelope on
stdout with exit 1. Existing text-mode invocations continue to
print the same human-readable `error: …` line on stderr (exit
unchanged for that path). Issue 2 (empty-prompt validator,
HTTP-path leak in builder errors) and the gold-standard observation
about inline allowed-values for `--preset` / `--model` are out of
scope for this bead.

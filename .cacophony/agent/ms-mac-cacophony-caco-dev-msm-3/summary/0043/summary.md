# Session summary — bd-7b8641 msg inbox --max-age validation

## Goal

Fix three UX gaps in `caco msg inbox`: raw HTTP 400 leak on bogus
--max-age, silent --max-age 0 vs error on --last 0 inconsistency,
and undocumented --tail/--limit precedence.

## Bead(s)

- `bd-7b8641` — caco msg inbox --max-age bogus leaks raw daemon 400

## Before state

- `--max-age bogus` leaked raw 'daemon response parse failed (HTTP
  400 Bad Request)' with no client-side validation
- `--max-age 0` silently returned empty results (vs --last 0 erroring)
- `--tail N --limit M` with N≠M silently used --limit, no warning

## After state

- `--max-age bogus` → friendly 'invalid --max-age value (expected positive integer of seconds)'
- `--max-age 0` → 'must be > 0 (always empty)' consistent with --last 0
- `--tail N --limit M` → stderr warning explaining --limit wins
- cargo test-small: 4299 pass; clippy clean

## Diff summary

- Commits: caf6e4326d85
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: 0 new (client-side validation; tested by existing arg-parser
  coverage + manual validation)
- Behavioural delta: three msg inbox UX gaps closed

## Operator-takeaway

Same pattern as the --since / --offset validator sweep — each
value-taking flag in caco needs client-side validation before sending
to daemon, or the raw HTTP error leaks. Issue 2 (negative-int parser
quirk) is systemic and deferred to a broader sweep.

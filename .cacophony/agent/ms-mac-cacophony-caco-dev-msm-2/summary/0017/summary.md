# Session summary — Parser bare-negative-number values (bd-8b4559)

## Goal

Stop the parser from rejecting bare negative-number flag values
(e.g. `caco tts set --speed -1`) as `error: unsupported flag: -1`.
General parser fix affecting any flag that takes a negative number
(`--speed`, `--offset`, `--depth`, `--limit`, `--priority`, etc).
Filed earlier this session as a follow-up from bd-56198d's
out-of-scope notes.

## Bead(s)

- `bd-8b4559` — caco CLI parser bare negative-number flag values
  rejected as 'unsupported flag' (e.g. --speed -1)
  (P2, bug, cli/parser/UX)

## Before state

```
$ caco tts set --speed -1
error: unsupported flag: -1
```

`parse_command_path`'s `next_is_value` predicate rejected any
token starting with `-` from being consumed as the preceding
flag's value. So `-1` fell through to the unsupported-flag
branch and the validator never saw the number.

Workaround that worked: `--speed=-1` (inline equals form).
Fixed by bd-205b39 + bd-56198d for that path.

## After state

```
$ ./target/debug/caco tts set --speed -1
error: --speed -1 is out of range; allowed: 0.25..=4
$ ./target/debug/caco tts set --speed -1.5
error: --speed -1.5 is out of range; allowed: 0.25..=4
$ ./target/debug/caco tts set --speed -bogus
error: unsupported flag: -bogus
$ ./target/debug/caco tts set --speed 1.25
TTS updated: speed: 1.25
```

The parser now routes numeric-looking tokens (`-?\d+(\.\d+)?`)
to the preceding flag's value. Validation of the parsed value
(range checks, etc.) remains the dispatch site's responsibility
— the parser only stops the unsupported-flag false-positive.
Non-numeric `-`-prefixed tokens (`-bogus`, `-r`, `-Lhost:port`)
still hit the unsupported-flag / passthrough branches as
before — the helper is intentionally narrow.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`:
    - New free function `is_numeric_value_token(token)` —
      classifies token as integer or decimal, optionally
      signed. Accepts `-1`, `-1.5`, `-.5`, `+3`, `100`, etc.
      Rejects `-`, `+`, `--foo`, `-r`, `-Lhost:port`,
      `-1abc`, `1.2.3`, empty.
    - In `parse_command_path`'s `flag if flag.starts_with("--")`
      arm, widened `next_is_value` predicate from
      `!next.starts_with('-')` to
      `!next.starts_with('-') || is_numeric_value_token(next)`.
- Tests: +2 / -0
  - `bd_8b4559_is_numeric_value_token_classification` — pins
    10 accept cases + 10 reject cases for the helper.
  - `bd_8b4559_parser_routes_bare_negative_number_to_preceding_flag`
    — end-to-end parser behaviour: `--speed -1`, `--speed -1.5`
    capture the negative as value; `--speed -bogus` still errors;
    `--speed 1.25` no regression.
- Test command:
  `cargo test -p caco-cli bd_8b4559` → 2 passed.

## Operator-takeaway

`--flag -N` now works for any declared flag, just like
`--flag=-N` does. No special-casing per command — single
helper at the parser layer fixes the entire CLI surface.

Two surfaces verified at the CLI:
- `caco tts set --speed -1` → out-of-range (validator fires)
- `caco tts set --speed -1.5` → out-of-range (validator fires)

Validation behaviour is unchanged — only the parser's pre-flight
filter loosened. So if you want to check whether your specific
flag has range validation, that's a separate concern (and
already covered for `--speed` by bd-205b39 / bd-56198d).

Honored constraints:
- No `cargo test --workspace`; targeted single-test run.
- No daemon changes — pure parser fix in caco-cli.
- Operator close-discipline rule: pre-close audit
  `git log origin/main --oneline --grep bd-8b4559` will be
  run before `caco bd close` so we don't false-close.

20th bead closed this session (cumulative). 13th in this turn.

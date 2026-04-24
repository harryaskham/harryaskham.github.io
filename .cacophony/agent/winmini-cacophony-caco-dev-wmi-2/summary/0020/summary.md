# Session summary — bd-0e8160: caco-cli + caco-web clippy clean (whole workspace now -D warnings green)

## Goal

Follow-up to bd-ff8249. Once that bead unblocked clippy
fail-fast, ~6 errors became visible in caco-cli and 1 in
caco-web. Fix them so `cargo clippy --workspace
--all-targets -- -D warnings` is fully green —
unblocking the bd-526670 post-reintegrate gate and
restoring the property to true on main.

## Before state

```
$ cargo clippy --workspace --all-targets -- -D warnings
caco-cli (lib): 6 errors
  - empty line after doc comment (lib.rs:22286)
  - duplicated attribute (lib.rs:73908, 76722)
  - useless conversion to the same type: CliError (10636)
  - enclosing Ok and ? unneeded (20558)
  - manual_contains (59336)
  - suspicious_double_ref_op on .clone() (25823)
caco-cli (lib test): + 5 errors
  - dead function warn_or_error_unknown_flags (9350)
  - dead function project_show_args_include_name_alias (76836)
  - 3x bool_assert_comparison
caco-web (lib test): 1 error
  - regex_creation_in_loops in a11y_lint.rs:315
... could not compile (caco-cli, caco-cli test, caco-web test)

$ cargo test-small
182 pass, plus 1 dead test (project_show_args_include_name_alias
was silently disarmed by stray duplicated #[test] above it).
```

## After state

```
$ cargo clippy --workspace --all-targets -- -D warnings
... Finished. (clean — whole workspace)

$ cargo test-small
test result: FAILED. 182 passed; 1 failed
  caco-web::confirm_overlay_layers_above_modal_overlay
  (separate broken-on-main being filed by po4-5; NOT mine)

$ cargo test -p caco-cli --lib project_show_args_include_name_alias
test result: ok. 1 passed
  (the bd-2c88ed regression guard now actually runs again)
```

## Bead(s)

- `bd-0e8160` — `caco-cli clippy --workspace -D
  warnings RED — 6 pre-existing errors revealed after
  bd-ff8249 cleared the fail-fast`

## Findings during fix

The pair of `error: duplicated attribute` warnings on
`#[test]` lines 73908 + 76722 turned out to be *real
test loss bugs*, not just stylistic noise:

The original source had two test functions stacked, each
with its own doc comment + `#[test]` attribute, in the
order:

```rust
#[test]                     // stray (never had a body)
/// bd-2c88ed: --name alias doc...
#[test]                     // legitimate, on bootstrap_dev test
fn bootstrap_dev_rejects_multiple_modes_per_invocation() { ... }

...

fn project_show_args_include_name_alias() { ... }   // ORPHANED!
```

The first `#[test]` had no body; the parser
re-associated it with the next `#[test]` it found
(`bootstrap_dev_*`), giving us the duplicated-attribute
warning *and* leaving `project_show_args_include_name_alias`
without an attribute, so it has been silently dead in
the test binary since whenever the two were merged.

**This is exactly the regression bd-2c88ed
(`PROJECT_SHOW_ARGS must include --name`) was meant to
guard against** — the test was written, committed, and
then immediately disarmed by an attribute mis-stack.
Restoring the `#[test]` attribute makes it execute
again. It passes — current `PROJECT_SHOW_ARGS` does
declare `--name` — so the bead's invariant is held *now*,
but for ~weeks it has been held by accident, not by
test enforcement.

## Diff summary

**caco-cli (lib.rs, ~9 fixes):**
- `lib.rs:22288` — empty line after doc comment between
  `MIN_TTS_SPEED` doc paragraphs → use `//` separator.
- `lib.rs:73908` — removed stray `#[test]` above doc
  comment (was duplicated-attribute on next test).
- `lib.rs:76722` — removed stray `#[test]` above doc
  comment for `bootstrap_dev_rejects_multiple_modes_*`.
- `lib.rs:76834` — RESTORED missing `#[test]` on
  `project_show_args_include_name_alias` — test had been
  silently inert since the stray attributes mis-stacked.
  bd-2c88ed's regression guard now actually guards.
- `lib.rs:9350` — `warn_or_error_unknown_flags` is a
  test-only convenience wrapper around
  `warn_or_error_unknown_flags_with_strictness`; gated
  with `#[cfg(test)]` rather than deleted (preserves
  test ergonomics; matches actual call-graph shape).
- `lib.rs:10636` — useless `.into()` on
  `Err(CliError::new(...))` (return type already
  `CliError`).
- `lib.rs:20558` — `return Ok(...?.map_err(...))` →
  `return ...map_err(...)` (Ok+? compose to identity).
- `lib.rs:25823` — `oid.clone()` on a `&&String`
  returned `&String` (no clone happened); fixed to
  `(*oid).clone()`.
- `lib.rs:59336` — `target_nodes.iter().any(|n| *n ==
  want)` → `target_nodes.contains(&want)` on `Vec<&str>`.
- `lib.rs:73911..73928` — three `assert_eq!(b, true|false,
  msg)` on `parse_tts_filter_flag` → `assert!(b, msg)` /
  `assert!(!b, msg)`.

**caco-web (a11y_lint.rs, 1 fix):**
- `no_native_title_attribute_in_dashboard_html` test
  compiled the regex inside the per-asset loop. Hoisted
  to before the loop (single regex, reused per asset).

## Verification

- `cargo clippy --workspace --all-targets -- -D
  warnings`: clean (whole workspace).
- `cargo test-small`: 182 pass, 1 fail —
  `caco-web::confirm_overlay_layers_above_modal_overlay`.
  This failure is a separate broken-on-main being
  filed by po4-5 (they messaged at 12:48Z: "modal-
  overlay z-index rule missing from CSS"). NOT mine.
- `cargo test -p caco-cli --lib
  project_show_args_include_name_alias`: 1 pass — the
  silently-dead test now actually runs and confirms
  `--name` is in `PROJECT_SHOW_ARGS`.
- `cargo test -p caco-cli --lib parse_tts_filter`: 2
  pass.
- `cargo test -p caco-cli --lib
  bootstrap_dev_rejects_multiple`: 1 pass.

## Operator-takeaway

bd-526670 post-reintegrate gate ('cargo check
--workspace --tests -D warnings') is now achievable
end-to-end on main:
  - caco-stt-bench, caco-tui, caco-stt-protocol,
    caco-config: green (bd-ff8249)
  - caco-cli, caco-web: green (this bead)

Health-log finding for next bd-8cf853 cycle: clippy
--workspace -D warnings GREEN at HEAD. The test-only
failure caco-web::confirm_overlay_* is being addressed
by po4-5.

Operator takeaway #2 (test loss): a `bd-2c88ed`-class
regression test sat silently dead for ~weeks because of
a stray duplicated `#[test]` attribute. The
`-D duplicate-macro-attributes` clippy lint flagged it;
this is a strong argument for keeping that lint
enabled in CI, not just allowing the easy fix to
suppress it. Already filed thought: a future test-
hygiene bead could add a static check that every
`fn name(...)` immediately preceded by a doc comment
inside `mod tests` carries an attribute (proc-macro or
clippy custom).

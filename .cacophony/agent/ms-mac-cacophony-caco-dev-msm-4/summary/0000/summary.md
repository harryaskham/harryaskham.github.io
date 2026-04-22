# bd-125b70 — fix `caco scp` arg passthrough

## Goal
Make `caco scp` (and other passthrough commands) honour the
`[<tool> args...]` contract advertised in their help: positional
sources are not silently dropped, and standard wrapped-tool flags
like `-r` actually pass through.

## Bead(s)
- bd-125b70 (P2 bug, test-user). Two issues filed together.
  Same family as bd-b76723 (warn-on-unknown-flag).

## Before state
- `caco scp --print helsinki:/tmp/foo /tmp/bar` printed
  `scp /tmp/bar` — the source path was silently consumed as
  --print's value because the parser peeked ahead, saw a non-flag
  non-subcommand token, and grabbed it as the flag's value. The
  user copy-pasted the printed command, hit a scp usage error
  with no hint that the source went missing. Workaround was to
  put `--print` *after* the positionals.
- `caco scp --print -r src dst` errored with
  `unsupported flag: -r`. The help advertises
  `[scp args...] SCP flags and path arguments. Remote paths use
  <node>:<path> form.` but the parser's blanket short-flag arm
  rejected anything starting with `-`.
- Same shape applied to `caco mosh`, `caco shell`, `caco exec`
  — every passthrough command suffered from both bugs.

## After state
- `parse_command_path` learns two helpers:
  - `is_passthrough_cmd(path)` — true iff the matched
    `CommandSpec.args` contains any `ArgSpec` whose `name`
    contains `args...]`.  Catches `[scp args...]`,
    `[mosh args...]`, `[shell args...]`, `[exec args...]`,
    `[caco-msg args...]` etc.
  - `known_flag_in_spec(path, flag)` — true iff `flag` matches an
    `ArgSpec.name` exactly.
- For known long flags under a passthrough command, parsing is
  forced to boolean: `--print` never consumes the next positional.
  Source path stays in positionals where it belongs.
- Unknown short flags (`-r`, `-O`, `-v`, …) under a passthrough
  command are routed into `passthrough_args` instead of erroring.
  The wrapped tool (`scp`, `mosh`, `bash`) is the authority on
  whether a given short flag is valid.
- Non-passthrough commands keep the strict
  `unsupported flag: <flag>` gate and the existing peek-as-value
  semantics for their own flags. No global behaviour change.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+115/-8):
  - `parse_command_path` (~L7989): new closures
    `is_passthrough_cmd` and `known_flag_in_spec`. Long-flag arm
    computes `force_boolean` and short-circuits the `next_is_value`
    peek when applicable. Short-flag arm splits into
    `passthrough_args.push(...)` (passthrough cmds) vs the legacy
    `Err("unsupported flag")` (everything else).
  - 3 new tests in `caco_cli::tests`:
    - `parse_command_path_scp_print_does_not_swallow_positional_source`
    - `parse_command_path_scp_routes_unknown_short_flags_to_passthrough`
    - `parse_command_path_non_passthrough_still_rejects_unknown_short_flags`
      (regression guard for the global unsupported-flag gate).

## Live verification
After binary rebuild, both reported reproductions:
```
$ caco scp --print helsinki:/tmp/foo /tmp/bar
scp -P 22 -i /Users/.../id_ed25519 harry@100.83.90.42:/tmp/foo /tmp/bar
$ caco scp --print -r helsinki:/tmp/foo /tmp/bar
scp -P 22 -i /Users/.../id_ed25519 harry@100.83.90.42:/tmp/foo /tmp/bar -r
```
Both print the correct command — no silent drops, no flag
rejection. (`-r` lands at the end of args; scp accepts flags in
either order.)

## Operator-takeaway
After roll, `caco scp` works as advertised: any flag order, any
scp short flag pass-through, no silent positional consumption.
Same fix automatically applies to `caco mosh`, `caco shell`,
`caco exec`, and any future command whose spec uses the
`[<tool> args...]` convention.

## Tests
- `cargo build -p caco-cli` — clean.
- `cargo clippy -p caco-cli --all-targets -- -D warnings` — clean.
- `cargo test -p caco-cli --lib parse_command_path_scp` — 2/2.
- `cargo test -p caco-cli --lib parse_command_path_non_passthrough` — 1/1.

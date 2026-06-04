# bd-facc71 (caco suggest S4) + bd-50c77e (broken-on-main clippy fix)

## bd-facc71: caco suggest S4 — parse-validate caco_cli items against the command tree

Slice 4 of 10 of the `caco suggest` epic (bd-a84d20). Adds a pure validation
helper so the `/suggest` generate endpoint (S3) can flag each suggested
`type=caco_cli` item `parse_valid: bool` without ever executing it.

### Implementation (crates/caco-cli/src/lib.rs, after spec_for_path)
- `pub fn validate_caco_cli(args: &[String]) -> Result<(), ParseError>`:
  - `args` are command tokens WITHOUT the leading `caco` (e.g. `["bd","show","--bead-id","x"]`).
  - Phase 1: greedily resolve the subcommand path from LEADING bare tokens via
    `spec_for_path`/`ROOT_COMMAND`; descent stops at the first flag or first
    non-subcommand bare token.
  - Phase 2: validate remaining tokens — `--flag` must be declared on the
    resolved command's `ArgSpec` table OR be a recognized global flag; a
    space-form known `--flag value` consumes its following non-flag token;
    bare tokens are positionals, allowed only when the command advertises
    `<arg>`/`[arg]`/`[args...]`.
  - Tolerates `--flag=value`, `--` passthrough, and opaque short `-x` flags.
- `pub enum ParseError { EmptyArgs, UnknownSubcommand{token,path}, UnknownFlag{flag,path} }`
  with Display + Error. Invalid items are still returned/persisted/displayed by
  S3 — this only explains WHY validation failed.
- `fn is_global_caco_flag` allow-list: `--json --help --global --config --gfx --no-gfx`
  (flags handled by the dispatcher outside per-command ArgSpec tables).
- Reuses the existing static `CommandSpec`/`ArgSpec` tree + `spec_for_path`
  (same tree MCP generation uses); no new command metadata.

### Tests (7, all passing)
valid path+flags; positional-accepting command (`config eval <expr>`); global
flags on any command; unknown subcommand; unknown flag; empty args; positional
rejected on a flags-only command (`bd show bd-abc123`).

CORE INVARIANT preserved: validation NEVER executes a command.

## bd-50c77e: broken-on-main clippy -D warnings fix (caco-cli)

`cargo clippy -p caco-cli -- -D warnings` failed at lib.rs:88051
(`orphans.sort_by(|x,y| y.reclaimable_bytes.cmp(&x.reclaimable_bytes))` ->
`unnecessary_sort_by`). Landed via 3d934aa10 (bd-57e873); slipped the
reintegration gate because fast-test-gate clippy is exit-code-only without
`-D warnings` (bd-68cabe). Fixed inline to unblock S4's clippy validation:
`orphans.sort_by_key(|orphan| std::cmp::Reverse(orphan.reclaimable_bytes))`.

## Coordination
Clean slice ownership confirmed with po4-1: S1 po4-1, S2 po4-3, S4 po4-2.
S4 has zero dependency on S1's config structs (it's over the static command
tree). Broadcast the broken-on-main clippy finding before taking it inline.

## Validation (queued)
- cargo check -p caco-cli — pass
- cargo clippy -p caco-cli -- -D warnings — pass (incl. the bd-50c77e fix)
- cargo test -p caco-cli --lib validate_caco_cli — 7 passed

## SPEC
caco suggest epic design (bd-a84d20); SPEC 8.x command-tree/MCP introspection
surface. S4 is library-only; S10 owns SPEC/README/docs for the feature.

## Diff
See the reintegration receipt for the landed squash SHA.

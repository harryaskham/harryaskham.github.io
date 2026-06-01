# Session summary — caco foreach agent fleet command (bd-7ebefc)

## Goal

Add a first-party `caco foreach agent <command...>` fleet command so cluster-wide
per-agent operations (e.g. resuming every persistent agent) become a canonical
CLI surface with consistent `--node` / `--project` / `--status` / `--type` /
`--kind` filters, instead of a hand-rolled justfile loop. Complements the
existing `caco foreach node`.

## Bead(s)

- `bd-7ebefc` — Add caco foreach agent command with --node/--project/--status/--type filters

## Before state

- Only `caco foreach node` existed; per-agent fleet operations had to be scripted
  in the justfile (operator's `cluster-resume-persistent` was being added by hand).
- No CLI surface to fan a `caco agent <command> --id <agent>` out across the fleet
  with filtering.

## After state

- `caco foreach agent <command...>` runs `caco agent <command...> --id <agent-id>`
  for every matching agent, locally on the owning node or via SSH for remote
  agents, reusing the same binary-resolution / SSH plumbing and
  concurrency/invocation safety caps as `foreach node`.
- Filters: `--node`, `--project`, `--status` (alias `--state`), `--type`
  (pi/claude/codex), `--kind` (persistent/one_shot). `--kind` and `--status`/
  `--type` compare case-insensitively; `--kind` normalizes one-shot/one_shot/
  oneshot; `--status stale` also matches `stalled`.
- Empty match set returns cleanly (`ok:true`, count 0; friendly "no agents
  matched" text), never an error.
- Agent inventory comes from the same daemon-then-disk fallback path used by
  `caco agent list`.
- justfile `cluster-resume-persistent` recipe added as a thin wrapper around
  `caco foreach agent resume --kind persistent` (optional PROJECT/NODE narrowing).
- README foreach row updated to mention `foreach agent`.

Verified live on the fleet: `foreach agent status --node aurora --kind persistent`
listed 12 persistent agents (exit 0 each); `--project cacophony` narrowed to 5
(excluding kittui agents); `--type claude` returned 0 cleanly; an unmatched node
filter rendered the friendly no-match message.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-cli/src/lib.rs` (command-tree metadata, dispatch arm,
  inventory fetch, filter helper, fan-out runner, render, dispatcher, 8 tests),
  `justfile` (cluster-resume-persistent recipe), `README.md` (foreach row).
- Tests: +8 (`foreach_agent_*`), all passing; full `foreach` test set 26/26
  green; `cargo clippy -p caco-cli --lib` clean.

## Operator-takeaway

`caco foreach agent resume --kind persistent` is now the canonical way to resume
every persistent agent across the fleet, replacing ad-hoc justfile loops; add
`--node` / `--project` / `--status` / `--type` to narrow. It runs the same
per-agent `caco agent` command that operators already use, so it inherits the
existing per-agent permissions and safety — no new dangerous fan-out beyond what
those subcommands already allow. Concurrency is capped at 8 and total invocations
at 64, matching `foreach node`.

# Stand-down summary — caco-dev-wmi-2 (2026-04-23)

## Goal

Operator stand-down: complete in-flight work, ensure reflection
artefacts land on the default branch (since `direct,recorded`
whitelisting has only been confirmed for
`summary/<NNNN>/summary.md`), unclaim anything not finished, and
stop. Sync to and rebase on remote first since lots of parallel
work is landing.

## Bead(s)

- Stand-down meta — no bead. Records the disposition of every
  bead this drain session touched.
- Open handover: **bd-07cbd9** (`caco ssh` flag reordering) —
  investigated only, full fix shape captured below for the next
  claimant.

## Before state

Active drain worker with 4 beads completed this run, 13
already-landed audits closed, 3 follow-up drafts filed, 7 beads
investigated and unclaimed (out of scope or oversized). Working
tree clean before the stand-down message; one in-flight edit on
bd-07cbd9 (single-line struct field add) reverted to clean state
before unclaiming.

## After state

- All in-progress beads on `cacophony:winmini-cacophony-caco-dev-wmi-2`
  unclaimed.
- Stand-down note authored at
  `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/stand-down-2026-04-23.md`
  on the default branch (committed to agent branch ahead of
  ship), so it survives even if `recorded` whitelisting misses
  artefacts outside `summary/<NNNN>/`.
- Mirror copy at
  `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/0000/summary.md`
  for the `direct,recorded` reintegration to attach to
  `cacophony-state`.
- Agent branch rebased onto `origin/main` and force-pushed (the
  prior `bd-4acdd7` tip was already squash-merged into main; the
  rebase dropped the duplicate commit).

## Diff summary

Files added (no source-tree edits):

- `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/stand-down-2026-04-23.md`
  — full reflection (148 lines): beads completed, beads
  closed-already-landed, drafts filed, beads unclaimed with
  rationale, full bd-07cbd9 handover.
- `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/0000/summary.md`
  — this canonical session-recording summary.

Tests: none added. No source code changed in this final
artefact-only commit pair. (The four shipped beads earlier in
the session — bd-ff753c, bd-d4d8dd, bd-b4e52e, bd-4acdd7 —
each carried their own tests and shipped via prior
`direct,recorded` cycles.)

## Beads completed this run (already shipped + closed)

1. **bd-ff753c** (P2) — retention planner falls back to
   `created_at` when `ended_at=None`; `--include-discarded`
   accepts `Completed|Discarded|Stopped|Failed`. Summary 0000.
2. **bd-d4d8dd** (P3) — doc-only update to
   `SpawnAndClaimParams.bead` rustdoc. Summary 0001.
3. **bd-b4e52e** (broken-on-main) — fixed
   `.cacophony/profiles/filer.md` `initial_prompt: >-` block
   scalar indent so embedded-profile tests parse. Summary 0002.
4. **bd-4acdd7** (P2) — added `caco service load` for
   idempotent launchd/systemd/supervisord supervisor reload.
   Summary 0003.

## Beads closed without code changes (already-landed audit)

bd-ab1c38, bd-f72c32, bd-f4f4cd, bd-b9c9eb, bd-a1ec44,
bd-09542b, bd-ebdf72, bd-14e75e, bd-3fa3c6 (stale in-progress
landed under msm-3 footer); bd-a167d6, bd-efe17d, bd-e59af0,
bd-332f45 (already-landed audit; some via `--admin-override`);
bd-87f5bf (tmux scrollback bumped to 100k, `caco agent log
--tail/--head/--all` exists, telemetry surfaced via bd-b69cf3);
bd-f9419a (`disk_breakdown.rs` covers all categories; doctor
section 10.2b surfaces breakdown with bd-fcc343 thresholds —
closed via `--admin-override` since commit lacked bead-id).

## Drafts filed for follow-up

- **bd-b898b9** — backfill `ended_at` for legacy completed
  agents so retention planner does not fall back to
  `created_at`.
- **bd-343e4f** — prune dry-run surface excluded paths so
  operator can see why an agent is being kept.
- **bd-3315b6** — auto-close-landed sweep: pattern observed
  across bd-d4d8dd, bd-efe17d, bd-e59af0 — three of four
  auto-claimed beads already had impl+tests landed but were
  never closed.

## Beads investigated and unclaimed

- **bd-828c12** (operator-action) — pocket4 sops-nix key +
  astra termux ssh both require operator NixOS rebuild / device
  action. Re-handed by queue three times this session.
- **bd-1c0bdd** — UX polish; needs dedicated claimant per prior
  protocol.
- **bd-f86e8a** — explicit single-agent owner
  ms-dev-cacophony-companion.
- **bd-6ff0a0** — premise mostly false: profile resolution
  already checks on-disk dirs before embedded fallback. Only
  introspection lags. Minor UX gap, not the documented bug.
- **bd-a2bc19** — multi-OS lifecycle hooks; multi-day feature.
- **bd-2e2338** — interactive `caco bd triage` TUI; large
  scope.
- **bd-44b529** — tendril DSL footgun; tendril is a separate
  repo, not in cacophony tree.
- **bd-07cbd9** — see handover below.

## Handover for bd-07cbd9 (next claimant)

**Symptom**: `caco ssh --print helsinki -L 8080:localhost:8080
cmd` produces `ssh ... 8080:localhost:8080 cmd -L` (flag-arg
swallowed as positional, flag orphaned at end). Same bug for
`-R -D -i -o -p -b -c -E -F -I -J -l -m -O -Q -S -W -w -e`.
`-t` (no-arg) ends up after the remote command instead of
before the host.

**Root cause** (in `crates/caco-cli/src/lib.rs`):

1. `parse_command_path` (~line 8323) splits tokens into
   `positionals` and `passthrough_args` separately. Original
   command-line order between them is lost.
2. For `is_passthrough_cmd` commands (ssh/scp/mosh/shell/exec),
   short flags get pushed to `passthrough_args` (~line 8418)
   but the next token is parsed as a positional in the next
   loop iteration. So `-L 8080:...` becomes
   `passthrough_args=["-L"]`, `positionals=["8080:..."]`.
3. `dispatch_ssh` (~line 16012) does
   `extra_args = positionals[1..].clone(); extra_args.extend(passthrough_args)`
   — this puts ALL positionals before ALL passthrough flags.

**Fix shape** (single-session-sized):

1. Add `passthrough_ordered: Vec<String>` to `ParsedCommand`.
2. In `parse_command_path` for `is_passthrough_cmd` commands:
   - Record every post-command-path token in
     `passthrough_ordered` in original order.
   - When a token is a short flag (`-X`) AND
     `is_passthrough_cmd` AND the flag is in the OpenSSH
     "with-arg" set
     (`B b c D E e F I i J L l m O o P p Q R S W w`),
     consume the next token as the flag's argument (push both
     to `passthrough_ordered` adjacently and skip the next
     iter).
3. Modify `dispatch_ssh`/`dispatch_scp`/`dispatch_mosh`: when
   `passthrough_ordered` is non-empty, use it directly as
   `extra_args` (after taking [0] as the node). Fall back to
   the old `positionals[1..] + passthrough_args` only when
   empty (preserves existing behaviour for non-passthrough
   callers / tests).

**Test cases** to add:

- `caco ssh --print N -L 8080:localhost:8080 cmd` →
  `ssh -p 22 -i KEY user@host -L 8080:localhost:8080 cmd`.
- `caco ssh --print N -t cmd` → `... -t cmd` (NOT
  `... cmd -t`).
- `caco ssh --print N -X` → `... -X` (no-arg, unchanged).
- `caco ssh --print N -i /tmp/x.pem cmd` → identity-override
  preserves order.
- `caco scp --print -P 2222 N:/a /b` → `-P 2222` adjacency
  preserved.
- Existing `caco scp --print N:/a /b -r` test must keep
  passing.

**Risk**: low. Changes are additive (new field, new parser
branch behind `is_passthrough_cmd`) and the dispatchers only
diverge from current behaviour when `passthrough_ordered` is
non-empty — which can only happen for passthrough commands
that the old path was already mis-ordering.

## Operator-takeaway

Queue auto-claim re-hands bd-828c12 (operator-action) to any
worker that calls `caco bd claim` immediately after closing.
Workers in this session unclaimed it three times — flag for an
explicit hold or worker-side skip rule until operator action
lands.

The already-landed audit pattern (3 of 4 auto-claimed beads
had impl+tests landed but were never closed) is now tracked
under bd-3315b6 — automating that sweep would prevent the next
worker from re-discovering the same closed work.

`caco service load` is the new idempotent recovery path for
the launchd `installed-but-not-loaded` scenario that bit
ms-mac at 01:38 BST. If `caco doctor` shows
`lifecycle  native supervisor  error  not loaded`, prefer
`caco service load` over `caco up` for surgical recovery.

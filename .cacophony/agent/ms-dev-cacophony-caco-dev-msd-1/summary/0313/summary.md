# Session summary — bd-f6a0f5 detect+surface half: persistent config-reload-gap ops finding

## Goal
Surface the silent config-reload gap where a replicated/newly-declared auto_start persistent never
materializes in an already-running daemon (the persistent sentinel doesn't hot-reload config), which
silently blocked release cadence 3+ hours on 2026-06-19 with NO operator signal. This is bd-f6a0f5's
explicitly-decomposed "cheaper, high-value half" (detect+surface).

## Bead(s)
- bd-f6a0f5 (P2 bug, daemon/config-reload/ops). complexity-5/decompose. This lands the detect+surface
  half. The harder HOT-RELOAD half is filed as bd-7da004 (filed first, per the auto-close-landed
  lesson — bd-f6a0f5's ID footers this commit, so the landed-sweep auto-closes it).

## Before state
Reintegration/config-declared persistents only became live after `caco restart`. `caco ops` / `caco
doctor` / `caco status` had NO signal that "config declares N persistents the running daemon doesn't
have." The block was silent + multi-hour.

## After state
- `caco ops` now emits a `persistents.config_reload_gap` finding: WATCH when config declares
  auto_start persistents (via Config::resolve_persistent_agents(local_node)) that have NO agent
  record (by deterministic id or decl name) in the running daemon; Routine otherwise.
- Pure helper `missing_auto_start_declared_persistents(declared, known_ids, known_names)`: filters to
  auto_start declared persistents absent from the known sets. Unit-tested.
- Finding builder `unmaterialized_persistents_finding(config, local_node, agents)`: computes
  declared (fresh config) vs known (daemon agent list), GUARDED against a degraded/empty agent list
  (ok:false / degraded:true / empty -> skip, so the bd-057f2e store-lock blanking can't false-flag
  every persistent). Surfaces the missing names + `caco restart` as the repair.
- Appended to the findings array after plan_findings (standalone-helper pattern, avoiding the
  OpsFindingInputs blast radius, like the reint-wedge finding). Read-only WATCH; ops never
  auto-restarts. caco-cli compiles clean; the helper test passes.

## Diff summary
- crates/caco-cli/src/ops_cmd.rs: missing_auto_start_declared_persistents (pure) +
  unmaterialized_persistents_finding (after reintegration_wedge_finding); appended in
  build_ops_snapshot's findings-array block; 1 unit test.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
A replicated/newly-declared auto_start persistent that the running daemon silently never materialized
(the config-reload gap) now shows up as a `caco ops` WATCH finding naming the missing persistents +
recommending `caco restart` — so it can't silently block release cadence for hours again. Read-only;
guarded against degraded agent-list false-positives. The actual hot-reload (self-heal without
restart) is the bd-7da004 follow-on.

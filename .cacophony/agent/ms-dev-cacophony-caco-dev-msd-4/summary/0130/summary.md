# Session summary — bounded ops status collectors

## Goal

Fix `caco ops status --project collective` so it remains responsive when one underlying ops input is slow or wedged, while preserving first-party recovery semantics and staying scoped to ops status responsiveness.

## Bead(s)

- `bd-6d53ae` — caco ops status times out while canonical msg/agent/bd surfaces are healthy

## Before state

- Collective overnight evidence reported `caco ops status --project collective` timing out at 120s and again at 90s, while canonical msg/agent/bd surfaces were healthy.
- Bounded repro from this checkout showed `caco ops status --project collective --json` produced no output within 15s.
- A direct bounded `caco msg inbox --project collective` returned in about 3s. From this Cacophony-bound worker checkout, cross-project `caco bd status --project collective --json` was one slow/hanging ops input, which was sufficient to wedge the entire ops aggregate.
- Additional Collective evidence arrived before landing: `caco agent list --project collective --json` produced no usable output under a 130s wrapper timeout/SIGKILL, while inbox, fleet summary, self status, and bd list succeeded. This is covered for ops responsiveness because agent list is now its own bounded `agents` collector; the underlying canonical agent-list slowness can be triaged separately if it persists after this slice lands.

## After state

- `caco ops status` now runs independent collectors concurrently behind one shared 8s status budget.
- If a collector times out, ops returns a partial degraded snapshot with that input marked `timed_out=true` instead of blocking the whole command.
- The findings list now includes `ops.collector_responsiveness`, which reports timed-out collectors and guides operators to inspect the named canonical surface directly.
- Collector threads use the same 16 MiB stack posture as heavy daemon/CLI status paths; a cargo-run repro initially found the default stack could overflow in the inbox collector, and the fix includes an explicit stack size.

## Diff summary

- Commits: agent-branch code commit for `bd-6d53ae` plus this summary commit; final mainline squash SHA is assigned during reintegration.
- Files touched: `crates/caco-cli/src/ops_cmd.rs`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-4/summary/pending/summary.md`
- Tests: +1 targeted unit test for timed-out collector degraded output.
- Validation: `cargo fmt --all -- --check` passed; queued job `tj-97148677` passed `cargo test -p caco-cli bd_6d53ae --lib`; queued cargo-run repro `tj-4fcdde7f` passed and proved the modified `caco ops status --project collective --json` returns `ok=true` with timed-out collectors marked in `inputs.*.timed_out` instead of hanging; post-rebase queued job `tj-7cfdea23` passed the unit test plus cargo-run repro.
- Behavioural delta: a slow ops sub-surface no longer makes the whole ops status command silent beyond controller probe budgets.

## Operator-takeaway

The ops status hang was an aggregation failure: one slow collector could block the whole snapshot. Ops status now behaves like an operator dashboard should — bounded, partial, and explicit about which collector needs direct follow-up.

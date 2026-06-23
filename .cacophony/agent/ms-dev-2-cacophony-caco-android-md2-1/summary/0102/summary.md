# bd-7b0bee increment 3 (completing): Android stats node filter + proportion bars

## Bead
bd-7b0bee (caco stats slice 6, caco-android) -- COMPLETE. Increments 1 (6ffc5c485)
+ 2 (77c1b699b) landed the foundation + throughput chart + window filter. This
increment 3 completes the full scope, so the bead CLOSES.

## Increment 3 (this land)
- ui/stats/StatsScreen.kt:
  - Compute/storage proportion bars (memory + disk used/total) -- the compute chart.
  - Per-project token proportion bars (share of grand total) -- the token chart.
  - Per-node section: a horizontally-scrollable node-filter chip row (All + each
    node) + per-node cards (cpu/mem/disk + a memory proportion bar), client-side
    filtered by the selected node.
  - New ProportionBar (Nord used/total Canvas bar) + StatsNodeChip composables.

## Full bead scope delivered (across increments 1-3)
- Consumes the canonical /api/v1/stats aggregate (slice 3).
- Cluster compute/storage, fleet token usage (per-project), beads throughput.
- Mobile charts: throughput time-series bars + compute/storage + token proportion bars (Nord palette).
- Filter-down: window (24h/72h/7d chips, server-side) + project (via the app project scope) + node (client-side per-node filter).
- All-projects view (cluster-wide when no project selected).

## Validation
Queued tj-4da5c725 (.#android-validation): compileDebugKotlin + StatsModelsTest ->
PASSED (exit 0). Increments 1+2 also compile+test-validated (tj-54db3ce6, tj-39c9bf4c).

## On-device visual QA (recommended, capable-host-gated)
The implementation is compile- + unit-validated. On-device VISUAL render QA of the
charts is recommended on a real device / hardware-accelerated builder; the headless
swiftshader emulator cannot reliably render heavy Compose (same constraint as
bd-bdbec9). Not a code blocker -- the wiring is compile-proven.

## Diff
See the reintegration receipt for the landed squash SHA.

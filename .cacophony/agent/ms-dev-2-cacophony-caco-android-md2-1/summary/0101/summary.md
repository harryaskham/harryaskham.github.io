# bd-7b0bee increment 2: Android stats charts + window filter (caco stats slice 6)

## Bead
bd-7b0bee (caco stats slice 6, caco-android). Increment 1 (foundation) landed
6ffc5c485. This is increment 2 (charts + window filter). NOT closing -- increment
3 (node filter-down + token chart) remains for full filter-down scope.

## Increment 2 (this land)
- state/StatsModels.kt: ProjectThroughput now parses the dense closed-bead time
  series (series.bucket unit + series.buckets[].count -> seriesCounts).
- ui/stats/StatsScreen.kt: a Nord ThroughputBars Canvas bar chart per project
  (mirrors the ThroughputPulse pattern in the Nord palette; bar height + alpha
  scale with the per-bucket count), plus a window filter chip row (24h / 72h /
  7d) that re-fetches getStats with the selected window.
- test/StatsModelsTest.kt: series parsing pin (bucket unit + counts).

## Validation
Queued tj-39c9bf4c (.#android-validation): compileDebugKotlin + StatsModelsTest
-> PASSED (exit 0).

## Remaining (increment 3, bd-7b0bee stays in_progress)
- Node filter-down dropdown (getStats already supports the node param).
- A token-usage bar chart (currently per-project cards).
- ms-dev android-builder visual validation of the rendered charts.

## Diff
See the reintegration receipt for the landed squash SHA.

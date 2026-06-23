# bd-7b0bee increment 1: Android stats screen foundation (caco stats slice 6)

## Bead
bd-7b0bee (caco stats slice 6, caco-android specialist lane): Android companion
stats screen consuming the canonical /api/v1/stats aggregate (slice 3, bd-d276e2,
landed). Building incrementally; this is increment 1 (foundation). NOT closing --
increment 2 (mobile charts + project/node/window filter-down) remains.

## Increment 1 (this land)
- state/StatsModels.kt: ClusterStats data model + pure parseClusterStats(body)
  parser for the SuccessEnvelope.data sections (scope, compute, per_node,
  throughput, tokens). Null-tolerant per the slice-3 graceful-degradation contract.
- connection/ConnectionManager.kt: getStats(project?, node?, windowHours?) GET
  helper, mirroring the getChatHistory idiom; formats project/node/window query
  params (window as "<n>h").
- ui/stats/StatsScreen.kt: minimal stats screen on the PRODUCTION NORD theme
  (per md2-0 -- production screens stay Nord; graduates to AURORA if blessed).
  Renders cluster compute/storage, fleet token usage (grand total + top projects),
  and beads throughput (total/hour + per-project 24h/7d) as Nord AccentCards.
  Fetches getStats() on connect.
- MainActivity.kt: nav wiring -- deep-link (stats/statistics), title, render
  switch, and a "Stats" MoreMenuItem in the System section (BarChart icon).
- test/StatsModelsTest.kt: parser pins (all sections, partial-section graceful
  degradation, corrupt body returns null).

## Validation
Queued tj-54db3ce6 (.#android-validation): :app:compileDebugKotlin +
:app:testDebugUnitTest --tests StatsModelsTest -> PASSED (exit 0). Compile covers
StatsScreen/getStats/nav/models wholesale.

## Remaining (increment 2, bd-7b0bee stays in_progress)
- Mobile charts (ThroughputPulse-style Canvas) for throughput + token trends.
- Project/node/window filter-down controls (getStats already supports the params).
- ms-dev android-builder visual validation of the rendered charts.

## Diff
See the reintegration receipt for the landed squash SHA.

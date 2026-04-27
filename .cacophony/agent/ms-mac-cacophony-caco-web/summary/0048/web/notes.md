# caco-web duty-cycle notes

- Board/inbox scan: initial caco board/inbox reads failed because local daemon API `127.0.0.1:11100` was temporarily unreachable during a restart/update window. The scan log captures `caco status --json` output showing recent restart activity.
- Post-observation retry: board reads recovered enough to check assignment and stale-dashboard title search. No bead was assigned to this agent. The queued stale managed-dashboard report from summary `0047` materialized as `bd-f74047 — Managed caco web serves stale dashboard assets after update`, status `in_progress`, assigned to `cacophony:ms-dev-cacophony-caco-dev-msd-4`, not this agent.
- Safety context: `bd-95cda5` had shown `closed` in the previous scan; this checkout still has preserved local summary commits and remains behind current `origin/main`. I did not rebase/reset/cherry-pick/reintegrate during this cycle.
- Observation driver: `caco-web-observe` current-checkout dev server against local daemon `http://127.0.0.1:11100`.
- Browser version surface: current checkout caco-web `v1.2.569`.
- Browser console: 0 total messages, 0 errors, 0 warnings.
- Network: completed observed browser requests returned 200 OK; one final `/api/v1/ui/snapshot` was still in-flight at browser close.
- Connection status: mostly `Connected`; later `Snapshot delayed` while the final bounded snapshot request was in-flight. Status text explicitly showed `beads: stale, agents: stale`, `Live SSE connected`, and `Snapshot degraded` age.
- Workspace/narrow route: observation flagged a scrollable workspace table (`AGENT STATE BEAD NODE RUNTIME USAGE ...`) as overflow. This is expected table scrolling, not fresh visual jank.
- Status hero: narrow and wide status hero remained unclipped (`h=330`, `scrollHeight=328`, `clipped=false`).
- Summaries route: requested current project-scoped bounded path `/api/v1/summaries?limit=10&offset=0&project=cacophony`, returned 200 in about 2.3s, and loaded detail for `ms-mac-cacophony-caco-tui/29` in about 0.3s.
- No new bead filed: the actionable stale managed-dashboard issue is already represented as `bd-f74047` and owned elsewhere; current-assets dashboard behavior was console-clean, visually stable, and using the corrected Summaries path.

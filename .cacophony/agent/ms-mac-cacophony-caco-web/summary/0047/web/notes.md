# caco-web duty-cycle notes

- Board/inbox scan: inbox readable. `bd-95cda5` is now `closed`; its manual PR guard was reported landed on `origin/main` as merge commit `adb5f24ff`, preserving guard commit `5976882de6d035f759181fa75dcbb021548b8639` as a main ancestor.
- Checkout caveat: this agent checkout still has preserved local summary commits and is behind `origin/main`; I did not rebase/reset/cherry-pick/reintegrate during this observation cycle.
- Assigned beads: no in-progress bead was assigned to this agent.
- Ready/open scans: no ready/open `caco-web`, `dashboard`, `web`, `browser`, `workspace`, `playwright`, `webui`, `summaries`, or `visual-polish` bead found. Text scans found no open web implementation bead; `bd-1cf76a` remains in progress for `ms-dev-cacophony-caco-dev-msd-4`, not this agent.
- Observation driver: `caco-web-observe --url http://127.0.0.1:11180` against the managed dashboard service rather than a temporary dev server.
- Managed dashboard version surface: `v1.2.567`, despite other fleet messages reporting macOS Test/Canary/stable at `v1.2.570` and current `origin/main` beyond this checkout.
- Browser console: 0 total messages, 0 errors, 0 warnings.
- Workspace/narrow route: only the known readable `✅ no choices` overflow/scroll-container entry appeared; no new Workspace jank was observed.
- Status hero: narrow and wide status hero remained unclipped (`h=330`, `scrollHeight=328`, `clipped=false`).
- Connection status: stayed in handled `Snapshot delayed` throughout the route pass.
- Summaries route: managed dashboard requested the stale unscoped `/api/v1/summaries?limit=200&offset=0` path, then showed `Session summaries unavailable: daemon proxy timed out` as handled retryable UI copy. This differs from current fixed caco-web behavior, which should use the project-scoped bounded request path.
- Network caveat: browser network log showed the Summaries request as HTTP 200 because the managed dashboard converted the daemon proxy timeout into handled JSON; two final `/api/v1/node` requests were aborted as the browser closed.
- Evidence warranted a focused bug: managed caco web appears to be serving stale dashboard assets after update, exposing old unscoped Summaries behavior.
- Bead filing attempt: attempted to file/claim `Managed caco web serves stale dashboard assets after update`. First attempt failed because local daemon/beads API at `127.0.0.1:11100` was unreachable; retry queued the create as outbox entry `outbox-019dcdf8-5ae4-7913-aa83-c270f5d0a76e` because beads primary was unreachable. No bead id was available yet, so nothing is claimed yet.

# caco-web duty-cycle notes

- Board/inbox scan: inbox was readable, but `bd-95cda5` and all assigned/ready/open bead scans were blocked by a declared beads-primary restart maintenance window on Helsinki. The reintegration freeze remains in effect per fleet messages.
- Safety context: direct recorded reintegration remains paused under `bd-95cda5`; this cycle is recorded locally only and is not reintegrated.
- Observation driver: `caco-web-observe` current-assets dev server against local daemon `http://127.0.0.1:11100`.
- Browser version surface: caco-web `v1.2.569`.
- Browser console: 0 total messages, 0 errors, 0 warnings.
- Network: all completed observed browser requests returned 200 OK; the final network dump ended with one in-flight `/api/v1/ui/snapshot` entry before browser close.
- Workspace/narrow route: no overflow entries observed.
- Connection status: stayed in handled `Snapshot delayed` throughout the route pass, matching active beads-primary maintenance/backpressure. The UI stayed console-clean and network-clean.
- Status hero: narrow viewport remained unclipped (`h=330`, `scrollHeight=328`, `clipped=false`).
- Summaries route: requested `limit=10&offset=0&project=cacophony`, returned 200 in about 15.6s, and loaded detail for `beelink-cacophony-technical-writer/70` in about 5.2s with no console errors. Slow but within bounded handled behavior.
- No bead filed: browser evidence showed no fresh focused caco-web defect. Snapshot-delayed and slow Summaries states were already explicit/handled during a known maintenance window, and bead filing/search was unavailable.

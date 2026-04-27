# caco-web duty-cycle notes

- Board/inbox scan: inbox was readable, but `bd-95cda5`, assigned-bead reads, and all ready/open web bead scans were blocked by a declared Helsinki beads-primary restart-maintenance window. I did not claim from stale state.
- Safety context: direct recorded reintegration remains paused under `bd-95cda5`; this cycle is recorded locally only and is not reintegrated.
- Observation driver: `caco-web-observe` current-assets dev server against local daemon `http://127.0.0.1:11100`.
- Browser version surface: caco-web `v1.2.569`.
- Browser console: 0 total messages, 0 errors, 0 warnings.
- Network: all observed browser requests returned 200 OK, including node detail, TTS, speech, summaries, stream, health, merge-queue, and snapshot requests.
- Workspace/narrow route: no overflow entries observed.
- Connection status: started as handled `Snapshot delayed`, briefly recovered to `Connected` on wide Status/Agents while live data populated, then returned to handled `Snapshot delayed` as snapshot requests hit the bounded delay. This matches backend/beads backpressure and stayed console-clean/network-clean.
- Status hero: narrow and wide status hero remained unclipped (`h=330`, `scrollHeight=328`, `clipped=false`).
- Summaries route: requested `limit=10&offset=0&project=cacophony`, returned 200 in about 2.6s, and loaded detail for `beelink-cacophony-technical-writer/70` in about 2.0s with no console errors.
- No bead filed: browser evidence showed no fresh focused caco-web defect. Snapshot-delayed/degraded and partial bead/stale-agent status were explicit handled states during known Helsinki maintenance/backpressure.

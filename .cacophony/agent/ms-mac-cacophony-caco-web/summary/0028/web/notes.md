# caco-web duty-cycle notes

- Board/inbox scan: no assigned in-progress caco-web bead; no ready/open web-labelled or text-matched caco-web bead found.
- Initial observation driver: `caco-web-observe` current-assets dev server against local daemon `http://127.0.0.1:11100`.
- Browser version surface: caco-web `v1.2.568`.
- Initial browser console: 0 total messages, 0 errors, 0 warnings.
- Initial network: all observed completed requests returned 200 OK.
- Workspace/narrow route: only observed overflow is the known readable `ws-status-choices` segment (`✅ no choices`).
- Status hero: narrow viewport `h=330`, `scrollHeight=328`, `clipped=false`.
- Evidence for bd-65e9e9: the initial project-scoped Summaries request `/api/v1/summaries?limit=10&offset=0&project=cacophony` took about 30.5s (`server.log`, req=29). The route showed generic `Loading…` / `Cold recorded-summary scans can take a few seconds`, making a healthy but slow scan look stuck.
- Implemented bd-65e9e9: after 8s of an initial list load with no rows, Summaries changes the header to `Still scanning…` and the empty-state copy to explain that large summary histories or daemon backpressure can take tens of seconds while preserving bounded retry/error behavior.
- After validation: Rust tests/checks passed; current-assets Playwright remained console-clean. During the after browser pass the daemon returned a handled Summaries 500 sentinel, which rendered a retryable route error without console noise.

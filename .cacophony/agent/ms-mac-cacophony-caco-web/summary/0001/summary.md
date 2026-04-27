# Session summary — caco-web recorded duty cycle

## Goal

Run the caco-web active duty cycle under the newly tightened recorded-artifact contract: first land the profile change that requires summary-rooted artifacts, then check inbox and assigned/ready web-dashboard beads, run a fresh current-assets Playwright dashboard observation pass, and preserve the evidence in this recorded summary before reintegration.

## Bead(s)

- `bd-09b32d` — caco-web profile should persist duty-cycle artifacts in recorded summaries

## Before state

- Failing tests: none known.
- Relevant metrics: caco-web profile update was committed locally but not yet durably landed; no current Playwright duty-cycle evidence existed under the new summary directory for this cycle.
- Context: Harry requested both the profile correction and another active caco-web duty cycle. The local daemon briefly had reachability/backpressure issues during board reads, so the cycle captured both successful and transient-failure board output in the summary artifacts.

## After state

- Failing tests: none observed for the profile-only change or dashboard observation.
- Relevant metrics: `bd-09b32d` was reintegrated with `direct,recorded` and closed; assigned in-progress list then returned `no beads found`; fresh current-assets Playwright observation reported `Total messages: 0 (Errors: 0, Warnings: 0)`.
- Context: The dashboard rendered the expected `Snapshot delayed` state while the daemon snapshot was slow/backpressured. Workspace, Status, Agents, Beads, Feed, Chat, Summaries, and keyboard help were inspected with bounded screenshots committed under `web/screenshots/`.

## Diff summary

- Commits: this observation-summary commit for the direct recorded reintegration that follows.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0001/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0001/web/*`.
- Tests: fresh `caco-web-observe` helper pass with current assets and `CARGO_BUILD_JOBS=2`; console clean; route shortcuts reached expected active views; Status hero measured `h=330`, `sh=328`, `clipped=false`.
- Behavioural delta: no product code changed in this commit. The durable delta is the recorded caco-web duty-cycle evidence now stored under the summary tree instead of only in `/tmp` or `.playwright-cli`.

## Embedded artefacts

- `web/board-and-inbox-scan.log` — inbox, assigned bead, close confirmation, label scans, text scan attempt, and caco web status.
- `web/observation.log` — full `caco-web-observe` output including route evaluations, console summary, network summary, and screenshot references.
- `web/server.log` — local current-assets dev server log for the observation pass.
- `web/console.log` — extracted console section showing `0` errors and `0` warnings.
- `web/network.log` — extracted network section showing observed dashboard requests returning `200 OK`.
- `web/notes.md` — compact no-bead rationale and key measurements.
- `web/page-2026-04-27T01-18-22-972Z.yml` — initial Playwright snapshot metadata.
- `web/screenshots/page-2026-04-27T01-18-34-748Z.png` — narrow Workspace screenshot.
- `web/screenshots/page-2026-04-27T01-18-39-481Z.png` — narrow Status screenshot.
- `web/screenshots/page-2026-04-27T01-18-43-872Z.png` — narrow Agents screenshot.
- `web/screenshots/page-2026-04-27T01-18-48-299Z.png` — narrow Beads screenshot.
- `web/screenshots/page-2026-04-27T01-18-52-598Z.png` — Feed screenshot.
- `web/screenshots/page-2026-04-27T01-18-56-922Z.png` — Chat screenshot.
- `web/screenshots/page-2026-04-27T01-19-01-407Z.png` — wide Workspace route screenshot.
- `web/screenshots/page-2026-04-27T01-19-05-824Z.png` — Summaries screenshot.
- `web/screenshots/page-2026-04-27T01-19-42-296Z.png` — keyboard help overlay screenshot.

## Operator-takeaway

The profile fix is now landed, and the very next caco-web observation followed the new discipline: artifacts are in the recorded summary tree, no additional focused browser-dashboard defect was found, and the clean evidence is ready to reintegrate with `direct,recorded`.

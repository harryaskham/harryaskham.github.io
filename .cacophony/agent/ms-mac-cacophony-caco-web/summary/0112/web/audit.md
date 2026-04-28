# caco-web comprehensive route audit

- Target: `http://127.0.0.1:62017`
- Viewports: narrow `390x844`, wide `1440x1000`
- Main routes inspected:
  - `#status`
  - `#agents`
  - `#beads`
  - `#feed`
  - `#chat`
  - `#nodes`
  - `#services`
  - `#projects`
  - `#choices`
  - `#notifications`
  - `#actions`
  - `#logs`
  - `#timeline`
  - `#summaries`
  - `#merge-queue`
  - `#workspace`
- Workspace pane types inspected:
  - `terminal`
  - `agents`
  - `beads`
  - `chat`
  - `logs`
  - `feed`
  - `source`
  - `status`
  - `services`
  - `nodes`
  - `projects`
  - `notifications`
  - `actions`
  - `timeline`
  - `choices`
  - `mergeQueue`
  - `speech`
  - `beadDetail`
  - `agentDetail`
  - `hooks`
  - `crons`
- Review the sibling observation log for route snapshots, overflow probes, console output, and network output.

## Audit result

- Console result: `Total messages: 0 (Errors: 0, Warnings: 0)` from `web/comprehensive-observation.log`.
- Network result: primary snapshot, node, UI stream, logs stream, merge queue, speech, node detail, and source-tree probes returned `200 OK`; the final snapshot was still pending at browser close.
- No additional focused caco-web defect was filed from this validation pass. Dense table/log/action-command overflow entries observed by the probe corresponded to intentional scroll containers or single-line ellipsis/truncation in dense UI chrome rather than a clear new browser-dashboard defect.
- The prior ad-hoc pane parsing issue is resolved: Workspace pane sweep selected `terminal` and `crons` exactly, without quoted values.

## Final helper proof after artifact-copy addition

- `web/comprehensive-observation-after-copy.log` is the final proof run for the implemented helper.
- It ended with `Total messages: 0 (Errors: 0, Warnings: 0)`.
- It logged a `copied playwright artifacts` section and self-copied screenshots/page snapshots into this summary directory.
- After pruning duplicate first-run screenshots, final bounded artifact counts are recorded in `web/artifact-counts.txt`.

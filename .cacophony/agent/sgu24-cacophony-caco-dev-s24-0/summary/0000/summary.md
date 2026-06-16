# Session summary — caco-mobile tooling-reliability lessons

## Goal

On a fresh wake the S24 light-dev worker had no assigned work and the ready
board was dominated by heavyweight native/app/daemon/compile beads that are
inappropriate for phone-class hardware. The node operator had explicitly asked
agents to fold real operational lessons into their profiles over time. This
session captures concrete, non-duplicate tooling-reliability lessons observed
this turn into the `caco-mobile` mixin so future low-power workers waste fewer
turns on the same transport/payload edges.

## Bead(s)

- No bound implementation bead. Operator-requested profile maintenance from the
  sgu24 node operator ("reintegrate any important lessons into your profiles
  over time; do not dedupe but self improve over time").

## Before state

- Failing tests: none touched (docs/profile-only change).
- `.cacophony/profiles/caco-mobile.md` covered local work / validation policy
  but had no guidance on tool-transport reliability for constrained nodes.
- Observed live this session: MCP `caco_*` calls flapped (Connection closed /
  Not connected / empty result); `caco agent status` returned ~79KB and
  exceeded the MCP response limit; a cross-node `caco bd list --ready` briefly
  returned `beads_proxy_unavailable` then succeeded on retry.

## After state

- Failing tests: none.
- `caco-mobile.md` gains a "Tooling reliability on constrained nodes" section
  documenting the MCP-flap-prefer-CLI pattern, oversized-read mitigations
  (`--count-only` / `--limit` / `--description-preview` / `--json` slicing), and
  the transient `beads_proxy_unavailable` retry guidance.
- `docs/profiles.html` unchanged: the source-light renderer uses frontmatter
  only, and `just docs-profiles-check-source-light` reports already up-to-date.

## Diff summary

- Code/content commits: profile commit on the agent branch; final landed squash
  SHA will come from the reintegration receipt.
- Files touched: `.cacophony/profiles/caco-mobile.md` (+22 lines, body-only).
- Tests: +0 / -0 / flipped 0.
- Behavioural delta: documentation-only; no runtime behavior change. Frontmatter
  untouched so no generated docs regen required.

## Operator-takeaway

On phone-class nodes the failures that waste the most turns are transport edges,
not logic: the MCP tool bridge flaps and large `caco` reads blow the response
limit. The durable fix is to prefer the `caco` CLI with bounded/sliced output
and to retry transient cross-node beads-proxy errors once. This is captured in
the caco-mobile profile so it compounds across future low-power workers.

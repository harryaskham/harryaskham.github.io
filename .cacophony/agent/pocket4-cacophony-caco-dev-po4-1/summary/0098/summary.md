# Session summary — Pi plugin tool-emission interference audit (bd-a7005c)

## Goal

A P0 bead asked for a full audit of every Pi plugin loaded by managed Cacophony
agents to find anything interfering with bash tool emission or tool-call
parsing — wrappers, duplicate emissions, or mishandled `antml:invoke` XML
prefixes — and to identify the root cause of observed "tools emitted with a bad
prefix" behavior.

## Bead(s)

- `bd-a7005c` — Audit pi plugins for tool emission interference (P0, task)
- `bd-97e24d` — Mitigate model-side antml:invoke malformed-tag loop
  (github-copilot/opus path) — filed as the actionable follow-up

## Before state

- Failing tests: none (investigation/audit bead, not a code-behavior change).
- Symptom under investigation: tool calls reportedly emitted with a bad/missing
  XML prefix, sometimes appearing to loop or not execute.
- No prior documentation of which Pi plugin surfaces are loaded or whether any
  of them wrap/rewrite tool emission.

## After state

- Failing tests: none.
- Audit complete and documented. Both plugin surfaces enumerated:
  repo `.cacophony/pi/*/extensions/*.mjs` overlays and operator
  `~/.pi/agent/extensions` + `~/.pi/agent/mcp-servers.json`.
- Conclusion: all Pi plugins are tool-emission-safe. Root cause of the symptom
  is model-side (a malformed `antml:invoke` namespace dropped to plain text in
  the github-copilot/claude-opus-4.8 path), not plugin-side. Quantified at 472
  malformed-tag turns across operator session logs.

## Diff summary

- Code/content commit: 7e8c445ec (final landed squash SHA will come from the
  reintegration receipt).
- Files touched: docs/design/pi-plugin-tool-emission-audit.md (new, 133 lines).
- Tests: +0 / -0 (docs-only audit deliverable; no behavior change).
- Behavioural delta: none in code. Produces a documented audit verdict plus a
  filed follow-up bead (bd-97e24d) for the model-side mitigation.

## Embedded artefacts

- None. (Audit evidence is summarized inline in the committed doc; raw operator
  session logs contain secret-adjacent material and were deliberately not
  copied into any artefact.)

## Operator-takeaway

The Pi plugin overlays and operator extensions are clean — the only tool
wrappers (bash via checkout-guard, read via image-guard) delegate correctly and
never touch the `antml:invoke` namespace, and nothing emits duplicate or
rewritten tool calls. The real cause of the "bad XML prefix" symptom is a
model-side formatting failure in the github-copilot/claude-opus-4.8 path, where
a namespace-stripped `<invoke>` block is emitted as plain text and silently not
parsed (it then loops). Fixing it belongs in the runtime/provider tool-format
path, tracked in bd-97e24d — not in any plugin.

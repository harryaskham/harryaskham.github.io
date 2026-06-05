# Summary — bd-6f61fb: detection signal for namespace-stripped <invoke> tool-call loop

## Goal
bd-6f61fb (P0, agent-tools/tool-parsing): agents emit tools as XML prefixed with
court/count tokens and get stuck in silent tool loops. Handed to po4-3 by po4-1
as the in-repo-actionable slice after the bd-a7005c / bd-5e4688 audits proved the
Pi plugins are clean and the malformed emission is model/provider-side.

## Scope determination (carried from the audits)
The court/count token is a prefix on the model's OUTGOING tool-call XML: on some
github-copilot + claude-opus turns the assistant emits a malformed tool call as
plain assistant text with the `antml:` namespace prefix on invoke/parameter
dropped (sometimes with a leading bare court/count token), so the harness never
parses it, no tool runs, and the model silently self-corrects next turn and
loops (docs/design/pi-plugin-tool-emission-audit.md). The actual emission fix
lives in the external Pi/provider serialization layer (@earendil-works/
pi-coding-agent), OUTSIDE this repo — a cacophony worker cannot land it here.
bd-6f61fb stays open + flagged operator-action/external-dependency for that
out-of-repo routing.

## What this slice delivers (the in-repo-actionable part)
A detection-only repo-owned Pi overlay that makes the silent loop OBSERVABLE
(audit recommendation #2), without rewriting tool emission/parsing:

- `.cacophony/pi/tool-emit-guard/extensions/caco-tool-emit-guard-utils.mjs`:
  pure, unit-testable helpers — `assistantText` (extract assistant text blocks),
  `hasNamespaceStrippedInvoke` (detect a bare `<invoke …>`/`<parameter …>` whose
  `antml:` namespace is stripped, while NOT flagging correctly-namespaced tags),
  `hasLeadingMalformedToken` (court/count line), and `classifyAssistantTurn`
  (malformed only when an invoke-shaped block survives in text AND no tool ran).
- `.cacophony/pi/tool-emit-guard/extensions/caco-tool-emit-guard.mjs`: subscribes
  to Pi's `agent_end`, finds the last assistant turn + whether a tool ran (a
  following toolResult message, or a parsed tool_use block), and on detection
  raises (1) a status-line marker, (2) a one-shot UI notification, and (3) a
  structured `[caco-tool-emit-guard] malformed_invoke_in_text count=… stopReason=…
  leadingToken=… excerpt=…` stderr diagnostic for logs/log-monitor aggregation.
  Adds `/tool-emit-guard` to show status + last detection. Best-effort and
  non-fatal (try/catch + runtime-warning), env escape hatch
  `CACO_PI_TOOL_EMIT_GUARD_DISABLED=1`. Never touches chat/inbox history.
- `.cacophony/profiles/pi-tool-emit-guard.md`: profile mixin (guard-mixin
  convention, like pi-checkout-guard) pointing `pi_extra_config_dirs` at the
  overlay.
- `.cacophony/agents/pi-common.yaml`: composes `pi-tool-emit-guard` into the
  default managed Pi stack (next to pi-checkout-guard).

Feasibility was confirmed against the installed Pi 0.78.1 extension API: the
`agent_end` event carries `messages: AgentMessage[]`, `turn_end` carries
`message` + `toolResults`, and assistant messages expose `stopReason` + text
content blocks — enough to classify the failure mode from an overlay.

## Tests / validation
- `caco-tool-emit-guard.test.mjs` (node --test): assistantText for string +
  block-array content, hasNamespaceStrippedInvoke on the malformed vs
  correctly-namespaced forms (the namespaced test string is built from parts so
  no rendering layer can strip its prefix — which is itself the phenomenon under
  test), hasLeadingMalformedToken, classifyAssistantTurn (malformed only when no
  tool ran), and lastAssistantTurn event extraction (toolResult => toolRan,
  malformed-text => not, no-assistant, parsed tool_use). GREEN.
- `caco config validate`: config valid across 15 nodes / 17 projects (the new
  mixin + stack resolve).
- docs/profiles.html regenerated via `just docs-profiles-build-source-light`;
  `docs-profiles-check-source-light` clean. AGENTS.md updated (default-stack list,
  overlay description, mixin list).
- No Rust source changed, so the reintegration fast-tests gate (test-small +
  check --workspace --tests + clippy) is unaffected by this JS/config/docs slice.

## Remaining
The model-side emission fix (bd-6f61fb core) remains out-of-repo (Pi/provider
runtime) and is correctly parked as operator-action/external-dependency. This
slice closes the audit's detection-signal recommendation in-repo so the loop is
no longer silent.

## Diff
See the landed squash commit in the reintegration receipt (code commits touching
.cacophony/pi/tool-emit-guard/, .cacophony/profiles/pi-tool-emit-guard.md,
.cacophony/agents/pi-common.yaml, AGENTS.md, docs/profiles.html).

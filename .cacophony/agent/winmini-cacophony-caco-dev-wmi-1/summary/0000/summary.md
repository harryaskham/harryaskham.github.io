# Session summary — bd-596416 Issue 3: bd dispatch --bead-id '' upfront guard

## Goal

Pin Issue 3 of the bd-596416 sweep: `caco bd dispatch --bead-id ''`
streamed through node-selection (wasted work) and emitted TWO log
lines echoing the empty quoted ID before the daemon's claim path
finally noticed. 16th empty-string-bypass surface (sub-family
'echo-empty-multi-line'). Add upfront guard.

## Bead(s)

- `bd-596416` — caco bd dispatch + expand + search + info sweep
  (P3 bug, multi-issue). Pins Issue 3. Issues 1-2, 5-6, 8-9 are
  POSITIVES (gold-standard PHASE PROGRESS streaming, 5 distinct
  required-flag pattern variants now catalogued). Issue 4 (bd
  expand --dry-run destructive bd-b76723, 6th surface) was just
  fixed by the bd-0f7e74 / bd-44f33a / bd-0c73b7 trio landed in
  this session — bd expand now refuses --dry-run upfront. Issue 7
  (required-flag short-circuits other validation) is normal CLI
  behaviour, not a bug to fix.

## Before state

```
$ caco bd dispatch --bead-id ''
▸ selecting node (candidates: ms-mac,ms-dev,helsinki,pocket4,winmini,beelink,sgu24,astra)…
✗ failed at agent.selecting_node: claim failed: claim_bead: bead not found:
bd dispatch: stream ended at phase 'agent.selecting_node' — claim failed: claim_bead: bead not found: ; falling back to verification
```

Three problems: (1) wasted node-selector work; (2) empty quoted
ID echoed across two log lines (16th empty-string-bypass); (3)
operator gets a confusing 'bead not found' framing instead of an
upfront flag-validation error.

## After state

```
$ caco bd dispatch --bead-id ''
error: --bead-id value cannot be empty for bd dispatch
```

## Diff summary

- 1 file changed, +12 / -0 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_bd_dispatch` checks `--bead-id` for whitespace-only
    / empty before any project resolution or stream setup.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco bd dispatch --bead-id ''` now produces the canonical empty-
string error before any work happens. 16 empty-string-bypass
surfaces patched individually now; bd-29c7e3 cross-cutting
validator helper increasingly justified.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.

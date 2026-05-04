# Session summary — caco-aks now uses a specialist persistent baseline without generic endless prompt text

## Goal

Take one of the remaining project-specific profile-stack audit beads from Harry's queue and fix it with config templating rather than ad hoc prompt surgery. The selected bead was `bd-1cd45f`, where the `caco-aks` persistent role was inheriting the generic `endless` mixin text from `persistent.yaml`, even though the `caco-aks` role profile already defines its own endless lifecycle and explicitly forbids generic auto-claim.

## Bead(s)

- `bd-1cd45f` — [profile-audit] endless generic claim instruction conflicts with caco-aks no-autoclaim

## Before state

- `.cacophony/agents/persistent.yaml` imports `base.yaml` and composes the generic `endless` mixin.
- The `caco-aks` declaration in `.cacophony/agents/cacophony_persistent.yaml` imported that shared `persistent.yaml` baseline.
- The `caco-aks` role profile in `.cacophony/profiles/caco-aks.md` already declares:
  - `persistent: true`
  - `lifecycle: endless`
  - explicit AKS-scoped work selection
  - explicit **NO autoclaim** / do not run bare `caco bd claim`
- That meant the reified prompt could still inherit the generic endless claim-next-task language from the shared persistent baseline, even though the role-specific AKS profile was trying to narrow it.

## After state

- Added a new shared config snippet:
  - `.cacophony/agents/persistent-specialist.yaml`
- This specialist baseline imports `base.yaml` and keeps the shared persistent cadence/restart/wait settings, but intentionally does **not** import the generic `endless` mixin.
- Added a templated value in `.cacophony/agents/cacophony_persistent.yaml`:
  - `values.persistent-specialist`
- Switched only the `caco-aks` declaration to that templated specialist baseline.
- Result: `caco-aks` still gets the shared worker/base stack, but its persistent/endless/no-autoclaim lifecycle wording now comes from the specialist profile itself rather than leaking in from the generic endless mixin.

## Diff summary

- Files touched:
  - `.cacophony/agents/persistent-specialist.yaml` (new)
  - `.cacophony/agents/cacophony_persistent.yaml`
- Config strategy:
  - introduced a reusable specialist persistent baseline
  - retargeted only `caco-aks` for this bead
- Validation:
  - `cargo build -p caco-config`
  - `cargo run -p caco -- --config .cacophony/config.yaml status`
- Behavioural delta:
  - `caco-aks` no longer composes the generic `persistent.yaml` endless prompt path
  - the AKS role’s own explicit no-autoclaim guidance becomes the primary lifecycle/work-selection contract

## Operator-takeaway

This is the same structural fix pattern as the observer-stack cleanup, but aimed at a specialist worker role instead of a pure observer. The right answer was not “patch the prompt later”; it was to stop forcing every persistent project role through one `persistent.yaml` stack. `caco-aks` now uses a more appropriate templated persistent baseline so its reified instructions line up with what the role is actually supposed to do.

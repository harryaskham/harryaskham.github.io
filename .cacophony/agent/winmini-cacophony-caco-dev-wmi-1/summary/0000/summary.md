# Session summary — bd-0c73b7 + bd-44f33a + bd-0f7e74: bd create / expand --dry-run / --preview implemented

## Goal

Three sibling noise beads filed when an operator typed
`caco bd create --preview "preview-test"`,
`caco bd create --dry-run "dry-run-test"`, and
`caco bd expand --dry-run "Test seed implementation"` — expecting
preview semantics — and discovered that ALL THREE flags were
silently dropped (warned via bd-b76723 unrecognised-flag noise)
and the destructive operation proceeded:
- bd create created stub beads with the literal title text.
- bd expand invoked the LLM expansion and created 1+ beads.

The accidental-test-user noise on the journal was the *symptom*;
the real bug is that the surface VIOLATES OPERATOR INTUITION on
a flag name that universally means 'do not commit'.

## Bead(s)

- `bd-0c73b7` — bd create --preview (P2 noise, but with bug
  underneath).
- `bd-44f33a` — bd create --dry-run (P2 noise, sibling).
- `bd-0f7e74` — bd expand --dry-run (P2 noise, sibling, LLM
  pathway).

All three are pinned and resolved by this commit. Closed not as
duplicate-of (each has its own bead create record + accidental
content) but with the proper fix landed against all three flag
surfaces.

## Before state

```
$ caco bd create --preview "preview-test"
warning: bd-b76723: `caco bd create` received unrecognised flag(s): --preview. These were ignored by the dispatcher.
created: bd-0c73b7 — preview-test                # surprise!

$ caco bd create --dry-run "dry-run-test"
warning: bd-b76723: ... --dry-run ignored ...
created: bd-44f33a — dry-run-test                # surprise!

$ caco bd expand --dry-run --text "Test seed implementation"
warning: bd-b76723: ... --dry-run ignored ...
expanded: 1 bead(s) created                      # surprise! (LLM pathway)
  bd-0f7e74 P3 [task] Test seed implementation
```

## After state

```
$ caco bd create --preview "preview-test"
would create (--dry-run) in project cacophony:
{
  "title": "preview-test"
}

$ caco bd create --dry-run "dry-run-test" --priority 1 --type bug --json
{"data":{"dry_run":true,"project":"cacophony","url":"...","would_create":{"priority":1,"title":"dry-run-test","type":"bug"}},"meta":{"action":"bd create (dry-run)"},"ok":true}

$ caco bd expand --dry-run --text "Test seed implementation"
error: bd expand --dry-run / --preview is not yet supported (the LLM expansion path always creates beads atomically); use 'bd create --dry-run' for individual bead previews, or run bd expand without the flag and inspect/close the created beads after
```

`bd create --dry-run` joins the bd-5f81fe `auto-close-landed`
gold-standard --dry-run cohort:
- Header explicitly names the operation + project
- `--json` envelope reflects `dry_run: true` in the payload
- Includes the planned URL + project as provenance fields
- Nothing reaches the daemon

`bd expand --dry-run` refuses with a clean affordance + suggested
workaround (use `bd create --dry-run` per-bead, or accept the
post-hoc cleanup). True preview support requires daemon-side
plumbing (the LLM call + bead create are atomic in one daemon
endpoint) — tracked separately as a follow-up.

## Diff summary

- 1 file changed, +85 / -3 (`crates/caco-cli/src/lib.rs`):
  - `BD_CREATE_ARGS`: added `--dry-run` + `--preview` ArgSpecs.
  - `dispatch_bd_create`: short-circuits POST when dry-run, emits
    text or JSON envelope.
  - `BD_EXPAND_ARGS`: added `--dry-run` + `--preview` ArgSpecs
    with 'not yet supported' summary.
  - `dispatch_bd_expand`: refuses upfront with affordance.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

The operator-intuition footgun (`--dry-run` / `--preview` SILENTLY
CREATING a bead) is closed across both surfaces. `bd create`
gains a real dry-run; `bd expand` returns a clean refusal with a
workaround. The three accidental noise beads (bd-0c73b7,
bd-44f33a, bd-0f7e74) are now closed cleanly.

This is also the canonical test-user-noise→shipping-fix pattern:
operator probes a flag, finds it's unsafe, files a 'noise' bead
documenting the accident — and the fix lands so the next operator
gets correct behavior.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.

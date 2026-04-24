# Session summary — bd-3c9e8f: bd expand --parent-epic + brief lift

## Goal

Per bd-8ea2d2 audit (29 'vague' beads attributed to ms-mac:_:node-
token were actually operator-driven LLM epic decompositions via
`caco bd expand --text "<freeform>"`). The vagueness is intrinsic
— the LLM takes a one-line brief and fans out 5-8 generic child
beads with boilerplate acceptance criteria. Two improvements:

(1) Add `--parent-epic <bd-id>` flag to `caco bd expand`. When
    set: each child bead gets a `parent_bead_id` link to the epic
    (so children render under the parent in graph/webapp tree
    views) + the operator's `--text` brief is lifted into each
    child's description as a 'Parent epic context' block listing
    sibling bead ids.

(2) Workers claiming a child bead can now SEE the broader
    decomposition without leaving the bead surface.

## Bead(s)

- `bd-3c9e8f` — bd expand --parent-epic + brief lift (P3 feature).
  Filed via bd-8ea2d2 audit.

## Before state

```
$ caco bd expand --text "macOS app with liquid glass"
expanded: 8 bead(s) created
  bd-aa8a1a P2 [task] Configure Nix builds for entire macOS app stack
  bd-bb1234 P2 [task] Implement liquid glass shader pipeline
  ... (no parent linkage, no operator brief context)

$ caco bd show --bead-id bd-aa8a1a
  description: Configure Nix builds...
  parent: -                                  # invisible context
```

Worker claiming bd-aa8a1a has no idea this came from a broader 8-
bead decomposition or what the operator originally asked for.

## After state

```
$ caco bd expand --parent-epic bd-EPIC --text "macOS app with liquid glass"
expanded: 8 bead(s) created
  bd-aa8a1a P2 [task] Configure Nix builds for entire macOS app stack
  bd-bb1234 P2 [task] Implement liquid glass shader pipeline
  ...

$ caco bd show --bead-id bd-aa8a1a
  description:
    ## Parent epic context
    Parent epic: bd-EPIC
    Operator brief: "macOS app with liquid glass"
    Filed via 'caco bd expand' on 2026-04-24T...; sibling beads: bd-bb1234 bd-cc5678 ...
    ---
    Configure Nix builds...
  parent: bd-EPIC
```

## Diff summary

- 1 file changed, +73 / -0 (`crates/caco-cli/src/lib.rs`):
  - `BD_EXPAND_ARGS`: added `--parent-epic` ArgSpec.
  - `dispatch_bd_expand`: forwards `parent_bead_id` +
    `lift_brief_into_children: true` to the daemon (graceful
    server-side opt-in for when the matching daemon-side support
    lands), AND a client-side post-create patch loop that PATCHes
    each child with the parent_bead_id link + lifted brief block
    if the daemon ignored the new fields. Skip-if-already-set
    detection means the loop is a no-op once the daemon honours
    the request.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco bd expand --parent-epic <bd-id> --text "..."` now wires
every generated child under the named epic and lifts the
operator's brief + sibling-id list into each child's description.
Workers claiming an LLM-decomposed child bead can SEE the broader
context without spelunking. Pre-pays for the bd-2a3aeb daemon-
native parent dependency model (the client-side patch loop
becomes a no-op once the daemon honours `parent_bead_id` +
`lift_brief_into_children` request fields).

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.

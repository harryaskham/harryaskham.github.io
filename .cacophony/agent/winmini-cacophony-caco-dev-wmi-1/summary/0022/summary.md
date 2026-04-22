# Session summary — caco operator-actions list (bd-8fe920)

## Goal

bd-6b7b30 identified that operator-action-blocked beads sat
>5h unnoticed. Step 1 (this bead): visibility via a dedicated
CLI surface.

## Bead(s)

- `bd-8fe920` — caco operator-actions list (P3 feature)

## Before state

- No dedicated surface for operator-required tasks.
- Operators had to manually `caco bd list --status open` and
  visually scan for `[operator-action]` in titles.

## After state

- New CLI: `caco operator-actions list [--project NAME] [--json]`
- Queries open beads via daemon API, client-side filters for
  titles containing `[operator-action]`.
- Text mode: sorted list with ID, priority, age, title.
  Exit code 1 when items exist (cron/alerting can key on it).
- JSON mode: structured `{count, beads[]}`.

## Diff summary

- Files touched (+90 / −0):
  - `crates/caco-cli/src/lib.rs`:
    - `operator-actions` CommandSpec branch with `list` leaf.
    - Dispatch arm for `[cmd, sub] if cmd == "operator-actions"
      && sub == "list"`.
    - `dispatch_operator_actions_list()` (~75 LOC).

## Verification

- `cargo build -p caco-cli`: clean.
- `cargo test-small`: 56 pass.
- `cargo clippy -p caco-cli --lib -- -D warnings`: clean.

## Operator-takeaway

```
$ caco operator-actions list
2 pending operator-action(s):

  bd-828c12  P2  8h ago  [operator-action] pocket4 needs sops-nix...
  bd-6b7b30  P2  15m ago [operator-action] no first-class secret...
```

Can be composed into cron:
```sh
caco operator-actions list || notify-send "operator actions pending"
```

Step 2 (periodic caco-ctrl speak) and step 3 (daemon-side
automation) are follow-ups from bd-6b7b30.

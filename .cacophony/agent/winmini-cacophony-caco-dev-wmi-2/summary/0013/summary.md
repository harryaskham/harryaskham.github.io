# Session summary — bd-dda312: empty --project '' rejected upfront across all callsites

## Goal

Address bd-dda312 Issue 8 (17th empty-string-bypass surface):
`caco changelog show --project ''` echoed the empty string back
inside the security-WHY error (`project '' is not configured;
bead operations must target a configured project ...`). The
upstream cause was that `resolve_project` and
`resolve_projects` both tested for "configured" without first
checking for empty.

Issues 5 (--limit 0 silent) and 6 (--limit bogus / '' HTTP-400
leak) of bd-dda312 were already fixed earlier in this drain via
bd-7abbba which added `validate_positive_limit("--limit", ...)`
to `dispatch_changelog_show`. Issue 7 (--limit -1 parser
ambiguity, 16 surfaces) and Issue 9 (--version alias hint) are
cross-cutting / UX work, separate beads.

## Bead(s)

- `bd-dda312` — `caco changelog show — gold-standard --since
  version validator (5th value-type confirmed shared); 5th
  surface security-WHY --project validator; gold-standard
  {ok,data,meta} envelope; env-aware default project — BUT
  4 drifts: --limit 0 silently empty; --limit bogus and
  --limit "" both leak HTTP-400+JSON-parse-error; 16th
  parser-ambiguity --limit -1; 17th empty-string-bypass
  --project echoes empty`.

## Before state

- `caco changelog show --project ''` →
  `error: project '' is not configured; bead operations must
   target a configured project ...` (echoes the empty quotes;
   17th member of the empty-string-bypass cohort).
- `caco bd list --project ''` → same.
- `caco ls --project ''` → same.
- `caco ps --project ''` → same.

The bypass propagated everywhere `resolve_project` /
`resolve_projects` was called because the empty-check was
missing on the explicit-flag path.

## After state

All four `--project` callsites now reject empty upfront with
the gold-standard 'must not be empty' template:

- `resolve_project` (--project flag arm).
- `resolve_projects` (multi-project explicit arm).
- `dispatch_ps` inline `--project` validator.
- `dispatch_ls` inline `--project` validator.

Each emits:

```
--project must not be empty (e.g. --project cacophony)
```

mirroring bd-9d3623's gold-standard "must not be empty for
caco X" template (msg snapshot --agent / build show --id /
secret get --path).

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `resolve_project`: empty-string guard at the top of the
    `--project` flag arm.
  - `resolve_projects`: same guard on the explicit arm.
  - `dispatch_ps`: same guard inside the inline
    `if let Some(p) = filter_project` block.
  - `dispatch_ls`: same guard inside the equivalent block.
  - 1 new test:
    `empty_project_flag_is_rejected_upfront_across_all_callsites`
    — source-greps the file for ≥4 occurrences of the
    `"--project must not be empty (e.g. --project cacophony)"`
    string so the four guards can't drift independently.
- `cargo test -p caco-cli --lib
   empty_project_flag_is_rejected_upfront_across_all_callsites`:
  pass.
- `cargo test-small`: 181 pass.

## Operator-takeaway

The empty-string-bypass cohort drops by one major class
today: any read-side surface that goes through
`resolve_project` / `resolve_projects` (which is most of
them — bd, build, test, release, summary, changelog,
project, image, etc.) now rejects `--project ''` with the
gold-standard wording.

The remaining empty-string-bypass surfaces called out in the
broader bd-5ae1ce tracker are typically independent
inline-flag validators (`--id`, `--note-id`, `--service`,
etc.) which need per-surface fixes — bd-3e39a0 (caco secret
get --path), bd-6dc352 (caco build show --id), and earlier
bd-9d3623 (msg snapshot --agent) are recent landings in
that family.

Out of scope deliberately:
- **Issue 7** (--limit -1 parser ambiguity, 16 surfaces) —
  flag-parser-level fix, treats negative-int values as
  flag tokens. Long-standing, needs its own bead (already
  called out in bd-6dc352).
- **Issue 9** (--version alias-hint for --since) — bd-9c55aa
  alias-suggestion family, distinct work.

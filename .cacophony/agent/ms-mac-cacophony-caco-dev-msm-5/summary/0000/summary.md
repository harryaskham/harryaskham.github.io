# Session summary 0000 — bd-02f7a3: caco bd reconcile-log

## Goal

Add the operator-facing surface that consumes the bd-4c0e22
reconciler-commit format (`prev=P new=Q delta=±D` + `⚠
DESTRUCTIVE` prefix), so listing recent reconciles or
specifically the destructive ones is one command not a
forensic dig.

## Bead(s)

- `bd-02f7a3` (filed by msm-5 this session as a bd-4c0e22
  follow-up).

## Before state

- Reconciler commits self-describe (per bd-4c0e22) but the
  only way to read them is `cd ~/.cacophony/beads/<p> &&
  git log --grep='sync reconcile'`. Not glanceable.

## After state

- New `caco bd reconcile-log` subcommand registered under
  `BD_SUBCOMMANDS`.
- Args: `--project` (required), `--limit` (default 50),
  `--destructive-only` flag, `--json` envelope.
- Reads `git log` on `paths.project_beads(project)`; no
  daemon round-trip.
- Parses each subject for `prev=N`, `new=N`, `delta=±N`,
  plus `imported/exported/skipped` counts.
- `--destructive-only` keeps `delta<0` or `⚠ DESTRUCTIVE`
  prefixed entries only.
- Tolerates legacy (pre-bd-4c0e22) format by skipping +
  surfacing a friendly explainer in the no-results case.
- Plain mode renders a fixed-width table; `--json` emits
  `{ok, data: {project, count, entries: [...]}}`.

## Diff summary

- Files (1): `crates/caco-cli/src/lib.rs` (+204 lines).
- `cargo build -p caco-cli`: clean.
- `cargo clippy -p caco-cli --all-targets -- -D warnings`:
  clean.
- Live-tested: `caco bd reconcile-log --project cacophony
  --limit 5` returns the legacy-format explainer correctly
  (no new-format commits exist yet since bd-4c0e22 is
  brand-new).

## Operator-takeaway

`caco bd reconcile-log --project cacophony` shows recent
reconciler activity. `--destructive-only` immediately shows
any reconciles that have net-deleted records. Doctor sensor
+ web/TUI surfaces deferred to follow-on slices.

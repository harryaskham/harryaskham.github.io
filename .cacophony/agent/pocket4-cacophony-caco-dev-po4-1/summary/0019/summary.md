# Session summary — bd-c3296e exclude raw Markdown from Pages artifact

## Goal

Stop the GitHub Pages workflow from republishing raw, unstyled
Markdown for internal-only docs trees that aren't meant to be
public-facing — so the Pages site only shows operator-facing
content with the shared site shell.

## Bead(s)

- `bd-c3296e` — [docs] Render or exclude raw Markdown from Pages artifact (P3 feature, gh-pages-visual-audit)

## Before state

- `.github/workflows/docs.yml` did `path: docs` for
  `upload-pages-artifact`, which uploaded the **entire** docs/ tree
  including 41 raw Markdown files.
- Internal-only directories (audits, epics, investigations, notes,
  postmortems, research, sweeps) were therefore reachable on
  Pages with no site shell, sidebar, favicon, fonts, or footer —
  unstyled and leaking in-flight engineering content.

## After state

- New `Stage public docs (bd-c3296e)` workflow step rsyncs docs/ to
  a runner-temp `$STAGE_DIR`, excluding the seven internal-only
  Markdown directories plus all top-level `*.md` files.
- It then selectively restores the eight operator-facing top-level
  Markdown docs that have rendered HTML siblings:
  `authorization-scopes`, `codespaces`,
  `controller-restart-windows`, `bead-submission-guidelines`,
  `logs`, `macos-development`, `notifications`,
  `reintegration-policy`. External links to those canonical
  reference docs continue to resolve.
- `upload-pages-artifact` is repointed at `${{ env.STAGE_DIR }}`.
- In-repo paths under `docs/` are unchanged so agent and operator
  discovery / source-of-truth references are unaffected.

## Diff summary

- Commit: a9534727b
- Files touched:
  - `.github/workflows/docs.yml` (+58, -1) — new staging step,
    repointed upload.
  - `crates/caco-web/src/tests.rs` (+45) — new
    `pages_workflow_excludes_internal_markdown_trees_bd_c3296e`
    asserts the staging step is present + bead-tagged, all seven
    internal trees excluded by name, and the upload step uses
    STAGE_DIR rather than raw docs/.
- Tests: cargo test-small 263/263 pass; `yaml.safe_load` on the
  workflow parses.

## Operator-takeaway

Pages users no longer see broken raw-Markdown views for internal
audits/epics/etc. The in-repo source-of-truth tree is preserved
unchanged for agent reading. The new test pins the exclude-list so
a drive-by edit can't quietly re-publish the internal Markdown
trees in a future workflow refactor.

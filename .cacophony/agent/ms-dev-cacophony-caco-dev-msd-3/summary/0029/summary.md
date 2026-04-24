# Session summary 0029 — bd-c7a85b slice 1: source-tree + file-content endpoints

## Goal

Backend half of source-code views in project workspaces. Lay the
HTTP surface so the caco-web pane work (slice 3) becomes a pure
front-end task.

## Bead(s)

- `bd-c7a85b` (P2 feature, slice 1 of 3)

## Before state

No HTTP surface for browsing the canonical project checkout. The
checkout exists at `$CACOPHONY_DIR/daemon/checkouts/<project>` and
is managed by `checkout.rs` (SPEC 14) but only the daemon and
agents touch it; the web has no read affordance.

## After state

Two new read-only endpoints registered in both the http and https
router blocks:

- `GET /api/v1/projects/{project}/source/tree[?path=<rel>]` — JSON
  list of immediate children of `<rel>` (default = root). Honours
  `.gitignore` via `git ls-files --cached --others
  --exclude-standard`. Capped at 5000 entries with `truncated=true`.

- `GET /api/v1/projects/{project}/source/file?path=<rel>` — text
  content under `data.content`. Refuses path traversal (canonicalize
  + starts_with), files >1 MiB, binary content (NUL byte heuristic),
  non-UTF-8.

New module `crates/caco-daemon/src/source_browse.rs` — ~22 KiB
including docs + 5 unit tests; one test is the load-bearing
`resolve_repo_path_refuses_dotdot_traversal` security pin.

## Diff summary

- `crates/caco-daemon/src/source_browse.rs`: new file, ~600 lines
- `crates/caco-daemon/src/lib.rs`: `pub mod source_browse;` + 4
  route entries (2 http + 2 https, matching the pattern of every
  other v1 endpoint).
- `cargo build -p caco-daemon`: clean.
- `cargo test -p caco-daemon --lib source_browse::`: 5/5 pass.

## Embedded artefacts

(none)

## Operator-takeaway

Slice 1 is reviewable on its own: a `curl` against any project
returns the file tree and any file's text content (within size +
binary limits). The web-side work in slices 2 and 3 can proceed
in parallel.

bd-c7a85b will remain in_progress this session; will append a
close note listing the slice-2/slice-3 follow-ups before close.

Three invariants self-audit (per operator's clarification just
now): (a) preserved via per-cycle merge -X theirs origin/main;
(b) preserved via reintegrate-only landings, all 13 closes
verified by `git log origin/main --grep`; (c) gap noted —
will start `git push --force-with-lease origin
HEAD:agent/<branch>` immediately before each reintegrate so the
remote agent branch reflects the actual local sequence rather
than just the post-squash result.

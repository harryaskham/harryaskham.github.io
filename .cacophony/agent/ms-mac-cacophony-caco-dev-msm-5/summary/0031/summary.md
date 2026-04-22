# Session summary 0031 — bd-d7fb98: caco bd create --attach (slice 1)

## Goal

Let agents/operators attach artefact paths (screenshot,
screen-recording) at bead-filing time so bug reports carry
diagnostic media without manual description editing.

## Bead(s)

- `bd-d7fb98` slice 1 — CLI flag only.

## Before state

- `caco bd create` had no first-class way to attach artefacts.
- Agents pasted paths into the description by hand or filed
  text-only bug reports (10x less useful per bd-d7fb98 motivation).

## After state

- `caco bd create --attach <path[,path...]>`:
  - Comma-separated path list.
  - Existence-checked before filing (clear error on bad path).
  - Appended to description as a markdown
    `## Attachments (bd-d7fb98)` block with one `- <path>` bullet.

## Diff summary

- Commit: `2ffbe581`.
- Files (1): caco-cli lib.rs.
- `cargo build` and `cargo clippy` for caco-cli + caco-daemon: clean.

## Operator-takeaway

Run `caco bd create --title "..." --attach screenshot.png`
(or `--attach a.png,b.mp4`) to embed artefact paths in the
filed bead. The capture step itself (`caco surface capture`) and
proper attachments table are bd-d7fb98 slice 2.

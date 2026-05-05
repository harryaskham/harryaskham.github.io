# bd-be632c: fix tui-fps-compare printf headers

## What changed

- Fixed `scripts/tui-fps-compare.sh` section header output.
- Replaced `printf '--- text baseline ---\n'` with an explicit `%s` format so Bash does not parse the leading `---` as an option.
- Did the same for the graphics section header.

## Why

The comparison wrapper added for graphics/text parity could fail at runtime before launching the benchmark legs because Bash `printf` treats a format string beginning with `-` as an option unless the format is supplied safely. This made the wrapper unusable despite passing high-level docs work.

## Validation

High-load bounded validation:

- `bash -n scripts/tui-fps-compare.sh`
- `scripts/tui-fps-compare.sh --help`
- `git diff --check`

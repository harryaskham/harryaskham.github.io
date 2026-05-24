# Session summary — rendered import parse diagnostics for config failures

## Goal

Make intermittent config parse failures like the reported `projects.midi2hid: duplicate entry with key "type"` actionable even when the visible source file does not show the duplicate. The implementation keeps the existing config import behavior but changes failures from templated/imported YAML parsing to include a rendered-context snippet around the parser-reported line.

## Bead(s)

- `bd-1055e3` — ms-mac config reload intermittently fails parsing projects.midi2hid duplicate type

## Before state

- Current checked-in `.cacophony/config.yaml` validated successfully from this checkout, so the exact duplicate-key symptom was not present in the agent tree.
- Import parsing in `crates/caco-config/src/imports.rs` rendered templated imported files before parsing them, but a parse failure was reported only as `ConfigError::Parse(path, serde_yaml::Error)`.
- For template-expanded imports, the YAML line/column in the serde error points into rendered text, not necessarily the original file, leaving operators to guess which import or expansion produced the duplicate line.

## After state

- Imported-file parse failures after template rendering now use `ConfigError::RenderedImportParse`.
- The error message includes the imported path, original serde parse error, and a compact rendered YAML snippet with line numbers, `>>` marker, and caret at the reported column.
- A targeted unit test exercises a templated `projects.midi2hid` duplicate `type` expansion and asserts the error includes rendered context pointing at the generated duplicate line.

## Diff summary

- Code/content commits: `547a78684` (`bd-1055e3: show rendered import parse context`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched:
  - `crates/caco-config/src/error.rs`
  - `crates/caco-config/src/imports.rs`
- Tests: +1 unit test / -0 / flipped 0
- Validation:
  - `caco config validate --config .cacophony/config.yaml --json`
  - queued `cargo test -p caco-config imports::tests::templated_import_parse_error_includes_rendered_context_bd_1055e3 -- --exact --nocapture` via `caco test run`, passed as `tj-b79def23`
- Behavioural delta: future duplicate-key failures from rendered imports should identify the generated YAML context directly instead of leaving only an original-file path and misleading line number.

## Operator-takeaway

The currently checked-in config already validates, so this slice fixes the diagnostic blind spot rather than removing a present duplicate from source. If the ms-mac symptom recurs from a dirty or templated canonical checkout, the next parse failure should show the rendered duplicate line and make the real source of the `type` collision visible.

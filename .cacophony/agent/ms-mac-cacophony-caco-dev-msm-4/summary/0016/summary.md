# Slice 14 — bd-3d8a4a: fix stt-bench.yml YAML parse error

## Goal

Unblock CI by fixing the unquoted-colon YAML parse error in `.github/workflows/stt-bench.yml` that has been failing every push to main.

## Bead(s)

- **bd-3d8a4a** (bug, P1, broken-on-main) — stt-bench.yml workflow YAML parse error.

## Root cause

Line 45 step name contained an unquoted colon:

```yaml
- name: Run bench (gate: overall<=12%, regress<=3pp, call-rel>=95%)
```

The bare `gate:` made YAML interpret the value as a mapping, producing "mapping values are not allowed here". GitHub Actions fast-failed every run on main with a 0-second workflow-file-issue.

## Before state

- Every push to main fast-failed stt-bench.yml in 0 seconds as a workflow-file-issue across at least 10 consecutive runs (run IDs 24946302565..24947230440).
- `python3 -c "import yaml; yaml.safe_load(open('.github/workflows/stt-bench.yml'))"` raised `mapping values are not allowed here`.

## After state

- Step name wrapped in double quotes; yaml.safe_load succeeds.
- stt-bench workflow can now parse and execute its real steps on each push.

## Fix

Wrap the step name in double quotes.

## Diff summary

```
 .github/workflows/stt-bench.yml | 2 +-
 1 file changed
```

`python3 -c "import yaml; yaml.safe_load(open('.github/workflows/stt-bench.yml'))"` clean.

## Operator-takeaway

stt-bench workflow can now parse and actually run instead of fast-failing on every push.

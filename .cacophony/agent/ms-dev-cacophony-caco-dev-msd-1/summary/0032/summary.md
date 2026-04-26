# Session summary — relaxed doctor disk threshold

## Goal

Respond to Harry's operator decision that the ms-mac disk warning threshold was too conservative and should only warn near a much higher usage level, around 95% used.

## Bead(s)

- `bd-69545e` — ms-mac host disk below 15% watch threshold after 1.2.559 restart

## Before state

- Failing tests: none.
- Relevant metrics: `caco doctor` treated host disks with less than 15% free as warning and less than 5% free as error, which flagged ms-mac at roughly 13% free despite more than 100 GiB still available.
- Context: the bead originally requested safe disk-pressure investigation, but the operator clarified the correct resolution was to relax the warning policy rather than clean up large-node storage immediately.

## After state

- Failing tests: none observed.
- Relevant metrics: `classify_disk_free_pct` now returns `ok` at 10% free, `warning` below 5% free, and `error` below 2% free, equivalent to warning at roughly 95% used.
- Context: the historical doctor coverage audit now documents the updated threshold as warn `<5%` free / error `<2%` free.

## Diff summary

- Commits: `aefded9b4`.
- Files touched: `crates/caco-cli/src/lib.rs`, `docs/audits/bd-4fcf9c-doctor-outage-coverage.md`.
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-cli classify_disk_free_pct_thresholds -- --nocapture`; `git diff --check`.
- Behavioural delta: doctor disk warnings are less noisy for large developer nodes; 13% free is no longer a warning condition.

## Operator-takeaway

The ms-mac warning was policy noise rather than an immediate cleanup emergency. Cacophony now warns only when the host is around 95% full and reserves error severity for very low free space.

# Session summary — audit-watcher fingerprint-direct dedup (bd-089f1d)

## Goal

Fix the audit-watcher dedup window silently degrading once the audit-bead pool grows past 200, so that 29 ECHILD-style duplicates (observed in bd-05b1f8 / bd-180c6d) cannot recur.

## Bead(s)

- `bd-089f1d` — audit-watcher: dedup query is bounded by limit=200 — fingerprint-specific lookup avoids N-bead-scan ceiling. Filed and self-claimed during this session.

## Before state

- `process_findings_routed` and `process_findings_with_debounce` did:
  ```
  list_beads(BeadFilter { label: Some("audit"), limit: Some(200), ... })
  ```
  then scanned the returned `Vec<Bead>.labels` for the finding's fingerprint. Once the cluster's audit pool grew past 200, older fingerprints fell outside the window and recurrences re-fired — observed in production as 29 separate ECHILD beads-sync drafts (bd-05b1f8, with bd-180c6d as the canonical instance).

## After state

- Per-finding fingerprint lookup:
  ```
  list_beads(BeadFilter { label: Some(finding.fingerprint), limit: Some(8), ... })
  ```
  Constant-time when the labels index is hit; zero false-negatives regardless of pool size.
- `open_bead_index` built lazily from the per-finding results; `dedup_seen` HashSet avoids redundant queries when the same finding repeats within a single batch.
- Removed the legacy `open_labels: Vec<Vec<String>>` shim (the public `is_duplicate_finding` helper is preserved for external callers but no longer used internally).
- New regression test `process_findings_deduplicates_past_legacy_200_audit_pool` files a canonical finding, then 250 distinct noise findings, then re-files canonical and asserts dedup still works (`new_beads_filed=0, deduplicated=1, appended=1`, canonical bead carries 'Seen again' note).

## Drive-bys

- `crates/caco-daemon/src/ui_stream.rs` — removed companion's re-introduced duplicate `tmux_history_limit/size` at one AgentSnapshot test fixture; added the missing pair at another that lacked them after companion's sweep.
- `crates/caco-cli/src/lib.rs:76796` — added `disable_hooks: None` to `Profile` literal after upstream bd-1d302a added the field.

## Verification

- `cargo test -p caco-daemon --lib audit::` — 74 / 0.
- `cargo test-small` — 209 / 109 / 739 / 295 / 18 / 2817 / 56 — all green.
- `cargo check --workspace --tests` — clean.

## Diff summary

- Commit: `151ec19b`
- Files: `crates/caco-daemon/src/audit.rs` (+92 / -90), `crates/caco-daemon/src/ui_stream.rs`, `crates/caco-cli/src/lib.rs`.
- Tests: +1 unit; 0 removed; 0 flipped.

## Out of scope

- A schema-level `auto_filer_signature` column / occurrence_count field (the broader bd-05b1f8 ask) — current label-based dedup is sufficient now that the lookup is fingerprint-direct.
- Threshold-based "re-create after N quiet days" for closed signatures.

## Operator-takeaway

The auto-filer cannot regress past 200 audit beads anymore; per-finding lookup is fingerprint-direct and pool-size-independent.

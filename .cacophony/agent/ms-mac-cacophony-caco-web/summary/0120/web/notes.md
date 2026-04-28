# caco-web bd-912d8d notes

- Started implementation after queued bead materialized and was claimed as bd-912d8d.
- Changed caco-web proxy so GET /api/v1/ui/snapshot upstream 5xx responses return console-clean 200 JSON sentinels with X-Caco-Upstream-Status.
- Added regression test proxy_translates_snapshot_5xx_to_console_clean_sentinels_bd_912d8d.
- Validation passed after one string-contract adjustment; see validation log.

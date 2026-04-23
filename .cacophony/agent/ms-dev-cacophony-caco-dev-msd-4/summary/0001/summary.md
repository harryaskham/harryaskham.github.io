# Session summary — bd-3bbc6f caco cron list/show improvements

## Goal
Make multi-line shell cron commands inspectable without --json + jq.

## Bead(s)
- `bd-3bbc6f` — caco cron list COMMAND column severely truncated

## Before state
- `caco cron list` truncated COMMAND column to ~50 chars, hiding 60+ line shell scripts behind ellipsis. No --name filter or focused-inspection subcommand existed; operators had to use --json + jq.

## After state
- `caco cron list --name <substr>` filters entries by name substring.
- `caco cron list --verbose` renders each entry as a multi-line block with full command output.
- `caco cron show --name <name>` adds a dedicated focused-inspection subcommand with exact match, JSON support, and empty-name guard.
- CommandSpec for cron list upgraded from mcp_leaf to full CommandSpec; cron show added.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+158 / -3): CRON_LIST_ARGS (--name, --verbose), CRON_SHOW_ARGS (--name), dispatch_cron_show, verbose rendering branch in dispatch_cron_list, CommandSpec registrations for list+show.
- Tests: 0 new (dispatch functions hit config read path). `cargo test-small` passes (146).

## Operator-takeaway
`caco cron list --verbose` and `caco cron show --name speaking-clock` now surface the full multi-line command body. No more --json + jq for basic inspection.

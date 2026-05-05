# bd-808078 — Fix beelink technical-writer Pi launch crash loop

## Summary
- Diagnosed `beelink-cacophony-technical-writer` Pi launch crash loop to inherited operator Pi home package activation.
- Remote evidence showed Pi exited immediately after bootstrap/readiness with:
  `Failed to load extension .../.pi-agent/git/github.com/heyhuynhgiabuu/pi-pretty/src/index.ts: Cannot find module '@shikijs/cli'`.
- Updated managed Pi bridge settings to emit `"packages": []`, so `pi-home` can still contribute first-party/explicit settings and warmed git cache paths without inheriting arbitrary operator-local package activations.
- Preserved inherited prompts/model-cache behavior while ensuring managed provider/model, extensions, skills, and packages remain deterministic.
- Updated SPEC/README/AGENTS to document that managed Pi runtimes must not inherit personal `packages` activations that can crash persistent agents before readiness/capture.

## Validation
- Remote beelink smoke: after setting the existing failed agent `.pi-agent/settings.json` `packages` to `[]`, `timeout 12s pi --continue ...` no longer exited immediately with the missing `@shikijs/cli` package error (timed out cleanly with no stderr instead of exit 1).
- `git diff --check` passed.
- `tj-27238fb3`: `cargo test -p caco-profile bridge_pi_settings_json_has_required_fields -- --test-threads=2` passed.
- `tj-131276ea`: `cargo test -p caco-daemon create_overrides_inherited_pi_enabled_models_with_managed_selector -- --test-threads=2` passed.

## Notes
- Did not spawn a duplicate replacement worker. The existing failed beelink persistent evidence under `/home/harry/.cacophony/agents/cacophony/beelink-cacophony-technical-writer` was preserved.
- Existing managed agents will pick up the permanent fix on profile/workspace re-materialization after this code is deployed; the first-party lifecycle path should recreate/recover the existing persistent rather than launching a duplicate.

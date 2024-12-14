serve:
	bundix -l && nix-shell

update:
	bundle config set --local force_ruby_platform true
	rm Gemfile.lock
	bundix -l

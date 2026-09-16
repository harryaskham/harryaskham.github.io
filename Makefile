.PHONY: dev serve build update

dev:
	./scripts/dev

serve:
	jekyll serve --watch --drafts

build:
	./scripts/build

update:
	bundle config set --local force_ruby_platform true
	rm -f Gemfile.lock
	bundix -l

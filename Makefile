serve:
	jekyll serve --watch --drafts

build:
	jekyll build

update:
	bundle config set --local force_ruby_platform true
	rm -f Gemfile.lock
	bundix -l

REBAR := rebar --config ./rebar_test_build.config

.PHONY: \
	travis_ci \
	fresh-build \
	compile \
	clean \
	deps \
	deps-get \
	deps-update \
	dialyze \
	test

all: \
	clean \
	deps \
	compile \
	test \
	dialyze

travis_ci: \
	deps \
	compile \
	test

fresh-build: \
	clean \
	compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps: \
	deps-get \
	deps-update

deps-get:
	@$(REBAR) get-deps

deps-update:
	@$(REBAR) update-deps

dialyze:
	@dialyzer ebin

test:
	@$(REBAR) ct skip_deps=true --verbose=0

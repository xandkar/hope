.PHONY: \
	fresh-build \
	compile \
	clean \
	dialyze \
	test

fresh-build: \
	clean \
	compile

compile:
	@rebar compile

clean:
	@rebar clean

dialyze:
	@dialyzer ebin

test:
	@rebar ct skip_deps=true --verbose=0

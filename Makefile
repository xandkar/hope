.PHONY: \
	fresh-build \
	compile \
	clean \
	dialyze

fresh-build: \
	clean \
	compile

compile:
	@rebar compile

clean:
	@rebar clean

dialyze:
	@dialyzer ebin

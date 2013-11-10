.PHONY: all compile clean dialyze

all: clean compile

compile:
	@rebar compile

clean:
	@rebar clean

dialyze:
	@dialyzer ebin

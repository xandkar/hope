.PHONY: all compile clean

all: clean compile

compile:
	@rebar compile

clean:
	@rebar clean

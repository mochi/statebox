REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)

.PHONY: all edoc test clean

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc skip_deps=true

test:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

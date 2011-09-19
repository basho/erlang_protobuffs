REBAR=`which rebar` || ./rebar

all: deps compile

get-deps:
	@$(REBAR) get-deps

compile: get-deps
	@$(REBAR) compile

test: compile
	@$(REBAR) skip_deps=true eunit
	@$(REBAR) skip_deps=true ct

clean:
	@$(REBAR) clean

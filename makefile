REBAR=`which rebar` || ./rebar

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile: get-deps
	@$(REBAR) compile

ct:
	@$(REBAR) skip_deps=true ct

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit ct

clean:
	@$(REBAR) clean

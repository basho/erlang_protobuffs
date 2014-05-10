REBAR=`which rebar || printf ./rebar`
REPO=protobuffs
all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test-deps:
	@$(REBAR) -C rebar.test.config get-deps compile

ct:
	./scripts/generate_emakefile.escript
	@$(REBAR) -C rebar.test.config skip_deps=true ct

eunit:
	@$(REBAR) -C rebar.test.config skip_deps=true eunit

test: test-deps eunit ct

clean:
	@$(REBAR) clean

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		ebin

dialyzer: compile
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin

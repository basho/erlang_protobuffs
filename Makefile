REBAR=`which rebar || printf ./rebar`
REPO=protobuffs
all: get-deps compile build

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

build: compile
	@$(REBAR) escriptize

ct:
	./scripts/generate_emakefile.escript
	@$(REBAR) skip_deps=true ct

eunit:
	@$(REBAR) skip_deps=true eunit

test: compile eunit ct

clean:
	@$(REBAR) clean

release: compile
ifeq ($(VERSION),)
	$(error VERSION must be set to build a release and deploy this package)
endif
ifeq ($(RELEASE_GPG_KEYNAME),)
	$(error RELEASE_GPG_KEYNAME must be set to build a release and deploy this package)
endif
	@echo "==> Tagging version $(VERSION)"
	@bash ./build/publish $(VERSION) validate
	@git tag --sign -a "$(VERSION)" -m "erlang_protobuffs $(VERSION)" --local-user "$(RELEASE_GPG_KEYNAME)"
	@git push --tags
	@bash ./build/publish $(VERSION)

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

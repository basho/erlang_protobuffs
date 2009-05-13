LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.2

all:
	mkdir -p ebin/
	(cd src;$(MAKE))

test: all
	prove t/*.t

test-eqc: all
	(cd t;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd t; $(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl

package: clean
	@mkdir erlang_protobuffs-$(VERSION)/ && cp -rf src eqct support t Makefile mysql.escript README.markdown erlang_protobuffs-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf erlang_protobuffs-$(VERSION).tgz erlang_protobuffs-$(VERSION)
	@rm -rf erlang_protobuffs-$(VERSION)/

install:
	mkdir -p $(prefix)/$(LIBDIR)/erlang_protobuffs-$(VERSION)/ebin
	for i in ebin/*.beam; do install $$i $(prefix)/$(LIBDIR)/erlang_protobuffs-$(VERSION)/$$i ; done

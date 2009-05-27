LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.3
PKGNAME=erlang_protobuffs

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
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin Makefile README.markdown scripts src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install:
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/ebin
	for i in ebin/*.beam ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done

LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.3.0
PKGNAME=erlang_protobuffs

all: app
	mkdir -p ebin/
	(cd src;$(MAKE))

app:
	sh ebin/$(PKGNAME).app.in $(VERSION)
    	
test: all
	prove t/*.t

test-eqc: all
	(cd t;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd t; $(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl ebin/*.app

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin Makefile README.markdown scripts src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install:
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/ebin
	for i in ebin/*.beam ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done

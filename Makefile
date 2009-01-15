LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`

all:
	mkdir -p ebin/
	(cd src;$(MAKE))

test: all
	prove -v t/*.t

clean:
	(cd src;$(MAKE) clean)
	(cd t; $(MAKE) clean)
	rm -rf erl_crash.dump simple_pb.* ebin/*.beam

dist-src:
	mkdir erlang_protobuffs-0.1/ && cp -rfv src support Makefile erlang_protobuffs-0.1/
	tar zcf erlang_protobuffs-0.1.tgz erlang_protobuffs-0.1

install: all
	mkdir -p ${LIBDIR}/protobuffs-0.1/{ebin,include}
	for i in ebin/*.beam; do install $$i $(LIBDIR)/protobuffs-0.1/$$i ; done

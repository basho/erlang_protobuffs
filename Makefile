all:
	mkdir -p ebin/
	(cd src;$(MAKE))

test: all
	prove -v t/*.t

clean:
	(cd src;$(MAKE) clean)
	(cd t; $(MAKE) clean)
	rm -rf erl_crash.dump simple_pb.* 

dist-src: clean
	tar zcvf erlang_protobuffs-0.1.tgz src/ support/ Makefile
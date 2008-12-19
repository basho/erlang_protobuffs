all:
	mkdir -p ebin/
	(cd src;$(MAKE))

test: all
	(cd t;$(MAKE))
	(cd t;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)
	(cd t; $(MAKE) clean)
	rm -rf erl_crash.dump 

dist-src: clean
	tar zcvf erlang_protobuffs-0.1_alpha1.tgz src/ support/ Makefile
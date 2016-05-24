ifdef ERL
  erl=$(ERL)
else
  erl=erl
endif

ifdef ERLC
  erlc=$(ERLC)
else
  erlc=erlc
endif

ifdef ESCRIPT
  escript=$(ESCRIPT)
else
  escript=escript
endif

ifdef HIPE
  HIPE_ERLC_OPTS = +'native' +'{hipe,[o3]}'
else
  HIPE_ERLC_OPTS =
endif

all: benchmarks-code

benchmarks-code: msg_pb.beam d_pb.beam

msg_pb.erl: msg.proto
d_pb.erl: d.proto

msg_pb.beam: msg_pb.erl
d_pb.beam: d_pb.erl

%_pb.erl: %.proto
	../protobuffs $<

%_pb.beam: %_pb.erl
	$(erlc) -Wall $(HIPE_ERLC_OPTS) +debug_info $(patsubst %,%,$<)

clean:
	$(RM) -f *_pb.erl *_pb.hrl *_pb.beam

benchmarks: benchmarks-code
	@echo CPU info
	@egrep '^model name' /proc/cpuinfo | head -1
	@egrep '^cache' /proc/cpuinfo | head -1
	@printf "# cores/threads : %s\n" `egrep -c '^processor' /proc/cpuinfo`
	@egrep '^bogomips' /proc/cpuinfo | head -1
	@echo
	$(erl) +V
	@echo
	./proto-bench \
          msg Message1 google_message1.dat \
          msg Message2 google_message2.dat \
	  d dm1 --multi d-msgs/d-msg-n*.dat --end-multi \
	  d dm1 d-msgs/d-all-concatenated.dat


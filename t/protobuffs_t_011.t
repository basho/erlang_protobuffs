#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(2),
    etap:is(protobuffs_compile:scan_file("t/empty.proto"), ok, "empty.proto created"),
	
	Rec = {empty,undefined,"testjuxsp108",undefined,undefined,undefined,
			undefined,undefined,undefined,undefined,undefined,undefined,
			undefined,undefined,undefined,undefined,undefined,undefined,
			undefined,undefined,undefined,undefined,undefined,undefined,
			undefined,undefined,undefined,undefined,
			undefined,undefined,undefined,undefined,undefined,undefined,undefined,
			undefined,undefined,undefined,undefined},
	
	Enc = empty_pb:encode_empty(Rec),
	Dec = empty_pb:decode_empty(Enc),
	etap:is(Dec, Rec, "match"),
	
	ok = file:delete("empty_pb.hrl"),
	ok = file:delete("empty_pb.beam"),

    etap:end_tests().

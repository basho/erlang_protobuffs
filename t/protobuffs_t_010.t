#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(1),
    etap:is(protobuffs_compile:scan_file("t/complex.proto"), ok, "complex.proto created"),
	
	ok = file:delete("complex_pb.hrl"),
	ok = file:delete("complex_pb.beam"),

    etap:end_tests().

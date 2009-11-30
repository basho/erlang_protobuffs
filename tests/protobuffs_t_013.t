#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../src -sasl errlog_type error -boot start_sasl -noshell

%% TODO: really just want to include_lib, but unfortunately thats not
%% possible at the moment, so need to hand include them
%% -include_lib("nested5_pb.hrl").
-record(first, {inner}).
-record(second, {inner}).
-record(first_inner, {foo}).

main(_) ->
    etap:plan(3),
    etap:is(protobuffs_compile:scan_file("nested5.proto"), ok, "nested5.proto created"),

    First = #first {
              inner = #first_inner { foo = false }
            },

    BinFirst = nested5_pb:encode_first (First),
    etap:is (nested5_pb:decode_first (BinFirst), First, "Encoded and decoded first"),

    Second = #second {
              inner = #first_inner { foo = true }
            },

    BinSecond = nested5_pb:encode_second (Second),
    etap:is (nested5_pb:decode_second (BinSecond), Second, "Encoded and decoded second"),

    etap:end_tests().

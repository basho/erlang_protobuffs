#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../src -sasl errlog_type error -boot start_sasl -noshell

%% TODO: really just want to include_lib, but unfortunately thats not
%% possible at the moment, so need to hand include them
%% -include_lib("nested2_pb.hrl").
-record(outer_middleaa_inner, {ival, booly}).
-record(outer_middlebb_inner, {ival, booly}).
-record(outer_middleaa, {inner}).
-record(outer_middlebb, {inner}).
-record(outer, {middleaa, middlebb}).

main(_) ->
    etap:plan(2),
    etap:is(protobuffs_compile:scan_file("nested2.proto"), ok, "nested2.proto created"),

    Outer = #outer{
              middleaa = #outer_middleaa {
                           inner = #outer_middleaa_inner {
                                      ival = 16#7aaabbbcccdddeee,
                                      booly = false
                           }
              },
              middlebb = #outer_middlebb {
                           inner = #outer_middlebb_inner {
                                      ival = 16#7aaabbbc,
                                      booly = true
                           }
              }
    },

    Bin = nested2_pb:encode_outer (Outer),
    etap:is (nested2_pb:decode_outer (Bin), Outer, "Encoded and decoded outer"),

    etap:end_tests().


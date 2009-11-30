#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../src -sasl errlog_type error -boot start_sasl -noshell

%% TODO: really just want to include_lib, but unfortunately thats not
%% possible at the moment, so need to hand include them
%% -include_lib("nested3_pb.hrl").
%% or
%% -include_lib("nested4_pb.hrl").
-record(outer, {middle}).
-record(outer_other, {bar}).
-record(outer_middle, {inner, other}).
-record(outer_middle_inner, {foo}).

main(_) ->
    etap:plan(4),
    etap:is(protobuffs_compile:scan_file("nested3.proto"), ok, "nested3.proto created"),

    Outer = #outer{
              middle = #outer_middle {
                           inner = #outer_middle_inner {
                                      foo = false
                           },
                           other = #outer_other {
                                      bar = true
                           }
              }
    },

    Bin = nested3_pb:encode_outer (Outer),
    etap:is (nested3_pb:decode_outer (Bin), Outer, "Encoded and decoded outer"),

    %% nested4 is just a rearranged nested3 so I can check that ordering does
    %% not affect things
    etap:is(protobuffs_compile:scan_file("nested4.proto"), ok, "nested4.proto created"),

    Outer2 = #outer{
              middle = #outer_middle {
                           inner = #outer_middle_inner {
                                      foo = false
                           },
                           other = #outer_other {
                                      bar = true
                           }
              }
    },

    Bin2 = nested4_pb:encode_outer (Outer2),
    etap:is (nested4_pb:decode_outer (Bin2), Outer2, "Encoded and decoded outer"),

    etap:end_tests().

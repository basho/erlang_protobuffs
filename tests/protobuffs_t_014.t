#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../src -sasl errlog_type error -boot start_sasl -noshell

%% TODO: really just want to include_lib, but unfortunately thats not
%% possible at the moment, so need to hand include them
%% -include_lib("boolean_pb.hrl").
-record(message, {booly, age}).

main(_) ->
    etap:plan(2),
    etap:is(protobuffs_compile:scan_file("boolean.proto"), ok,
            "boolean.proto created"),

    Message = #message { age = 35 },

    Bin = boolean_pb:encode_message (Message),

    %% message with default to compare against
    RealMessage = #message { age = 35, booly = false },

    etap:is (boolean_pb:decode_message (Bin), RealMessage,
             "Encoded and decoded message"),

    etap:end_tests().


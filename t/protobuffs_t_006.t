#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-record(location, {region, country}).
-record(person, {name, address, phone_number, age, location}).

main(_) ->
    etap:plan(1),
    etap:is(protobuffs_compile:scan_file("t/simple.proto"), ok, "simple.proto compiled"),
    compile:file("simple_pb.erl"),

    LocationBinData = erlang:iolist_to_binary([begin
        protobuffs:encode(Pos, Value, Type)
    end || {Pos, Value, Type} <- [{1, <<"California">>, string}, {2, <<"USA">>, string}]]),

    Data = [
        {1, <<"Nick">>, string},
        {2, <<"Mountain View">>, string},
        {3, <<"+1 (000) 555-1234">>, string},
        {4, 25, int32},
        {5, LocationBinData, bytes}
    ],
    BinData = erlang:iolist_to_binary([protobuffs:encode(Pos, Value, Type) || {Pos, Value, Type} <- Data]),
    #person{
        name = <<"Nick">>,
        address = <<"Mountain View">>,
        phone_number = <<"+1 (000) 555-1234">>,
        age = 25,
        location = LocationBinData
    } = simple_pb:decode_person(BinData),
    BinData = simple_pb:encode_person(#person{
        name = <<"Nick">>,
        address = <<"Mountain View">>,
        phone_number = <<"+1 (000) 555-1234">>,
        age = 25,
        location = #location{ region = <<"California">>, country = <<"USA">>}
    }),
    BinData = simple_pb:encode_person(#person{
        name = <<"Nick">>,
        address = <<"Mountain View">>,
        phone_number = <<"+1 (000) 555-1234">>,
        age = 25,
        location = LocationBinData
    }),
    etap:end_tests().

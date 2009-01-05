#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-record(locationb, {region, country}).
-record(personb, {name, address, phone_number, age, location}).

main(_) ->
    etap:plan(2),
    etap:is(protobuffs_compile:scan_file("t/hasdefault.proto"), ok, "hasdefault.proto created"),
	etap:is(compile:file("hasdefault_pb.erl", [{outdir,"./ebin"}]), {ok, hasdefault_pb}, "hasdefault_pb.erl compiled"),

	Region = <<"California">>,
	Country = <<"USA">>,
    LocationBinData = erlang:iolist_to_binary([begin
        protobuffs:encode(Pos, Value, Type)
    end || {Pos, Value, Type} <- [{1, Region, string}, {2, Country, string}]]),

    Data = [
        {1, <<"Nick">>, string},
        {2, <<"Mountain View">>, string},
        {3, <<"+1 (000) 555-1234">>, string},
        {5, LocationBinData, bytes}
    ],
    BinData = erlang:iolist_to_binary([protobuffs:encode(Pos, Value, Type) || {Pos, Value, Type} <- Data]),

    Person = #personb{
        name = <<"Nick">>,
        address = <<"Mountain View">>,
        phone_number = <<"+1 (000) 555-1234">>,
        age = 25,
        location = #locationb{region = Region, country = Country}
    },

 	#person{
        age = 25
    } = hasdefault_pb:decode_personb(BinData),
 
    %% BinData = simple_pb:encode_person(Person),

	ok = file:delete("simple_pb.erl"),
	ok = file:delete("simple_pb.hrl"),

    etap:end_tests().

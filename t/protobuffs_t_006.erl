-module(protobuffs_t_006).
-export([start/0]).

-record(location, {region, country}).
-record(person, {name, address, phone_number, age, location}).

start() ->
    etap:plan(1),
    etap:is(protobuffs_compile:scan_file("simple.proto"), ok, "simple.proto compiled"),
    compile:file("simple_pb.erl", [{outdir,"./ebin"}]),

	Region = <<"California">>,
	Country = <<"USA">>,
    LocationBinData = erlang:iolist_to_binary([begin
        protobuffs:encode(Pos, Value, Type)
    end || {Pos, Value, Type} <- [{1, Region, string}, {2, Country, string}]]),

    Data = [
        {1, <<"Nick">>, string},
        {2, <<"Mountain View">>, string},
        {3, <<"+1 (000) 555-1234">>, string},
        {4, 25, int32},
        {5, LocationBinData, bytes}
    ],
    BinData = erlang:iolist_to_binary([protobuffs:encode(Pos, Value, Type) || {Pos, Value, Type} <- Data]),

    Person = #person{
        name = <<"Nick">>,
        address = <<"Mountain View">>,
        phone_number = <<"+1 (000) 555-1234">>,
        age = 25,
        location = #location{region=Region, country=Country}
    },

 	Person = simple_pb:decode_person(BinData),

    BinData = simple_pb:encode_person(Person),

	ok = file:delete("simple_pb.erl"),
	ok = file:delete("simple_pb.hrl"),

    etap:end_tests().

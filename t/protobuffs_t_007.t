#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-record(location, {region, country}).
-record(person, {name, address, phone_number, age, hobbies, locations}).

main(_) ->
    etap:plan(1),
    etap:is(protobuffs_compile:scan_file("repeater.proto"), ok, "repeater.proto compiled"),
    compile:file("repeater_pb.erl", [{outdir,"./ebin"}]),

	Location1 = #location{region = <<"Lyon">>, country = <<"France">>},
	Location2 = #location{region = <<"Reykjavik">>, country = <<"Iceland">>},

    Person = #person{
        name = <<"Jake">>,
        address = <<"San Francisco">>,
        phone_number = <<"+1 (000) 555-1234">>,
        age = 25,
		hobbies = [<<"water colors">>, <<"cooking">>],
        locations = [Location1, Location2]
    },

	BinData = repeater_pb:encode_person(Person),
	
 	Person = repeater_pb:decode_person(BinData),

	ok = file:delete("repeater_pb.erl"),
	ok = file:delete("repeater_pb.hrl"),
	
    etap:end_tests().

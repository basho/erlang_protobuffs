#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../src -sasl errlog_type error -boot start_sasl -noshell

%% TODO: really just want to include_lib, but unfortunately thats not
%% possible at the moment, so need to hand include them
%% -include_lib("nested1_pb.hrl").
-record(person_phonenumber_phonetype, {mobile, home, work}).
-record(person_phonenumber, {number, type}).
-record(person, {name, id, email, phone}).

main(_) ->
  etap:plan(2),
  etap:is(protobuffs_compile:scan_file("nested1.proto"), ok, "nested.proto created"),

  Person = #person{
             name = "Anthony",
             id = 655350,
             email = "anthony@example.com",
             phone = [#person_phonenumber{
                       number = "+1 (000) 555-1234",
                       type = #person_phonenumber_phonetype { mobile = 25 }
                     }]
           },

    Bin = nested1_pb:encode_person (Person),

    etap:is (nested1_pb:decode_person (Bin), Person, "Encoded and decoded person"),

  etap:end_tests().

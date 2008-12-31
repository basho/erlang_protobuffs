#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(1),
    Data1 = [{1, 1, uint32}, {2, 5600, int32}, {3, <<"testing">>, string}],
    Set1 = erlang:iolist_to_binary([
        protobuffs:encode(Pos, Data, Type)
    || {Pos, Data, Type} <- Data1]),
    etap:is(protobuffs:decode_many(Set1), [{1, 1}, {2, 5600}, {3, <<"testing">>}], "decode_many"),
    etap:end_tests().

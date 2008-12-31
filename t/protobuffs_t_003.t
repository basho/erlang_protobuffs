#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(3),
    etap:is(protobuffs:decode(<<8, 1>>, uint32), {{1, 1}, <<>>}, "1 - 1"),
    etap:is(protobuffs:decode(<<16, 1>>, uint32), {{2, 1}, <<>>}, "2 - 1"),
    etap:is(protobuffs:decode(<<24, 1>>, uint32), {{3, 1}, <<>>}, "3 - 1"),
    etap:end_tests().

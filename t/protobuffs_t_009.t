#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(2),
    etap:is(protobuffs:encode(1, true, bool), [8, 1], "1, true, bool"),
    etap:is(protobuffs:encode(19, false, bool), [<<129,24>>,0], "19, false, bool"),
    etap:end_tests().

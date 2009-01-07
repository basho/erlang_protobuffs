#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell

main([Input]) ->
	ok = protobuffs:generate(Input).
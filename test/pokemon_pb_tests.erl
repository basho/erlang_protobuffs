%%%-------------------------------------------------------------------
%%% File    : pokemon_pb_tests.erl
%%% Author  : David AAberg <david_ab@RB-DAVIDAB01>
%%% Description : 
%%%
%%% Created : 13 Aug 2010 by David AAberg <david_ab@RB-DAVIDAB01>
%%%-------------------------------------------------------------------
-module(pokemon_pb_tests).

-include_lib("eunit/include/eunit.hrl").

with_default_test_() ->
    [?_assertEqual(undefined,pokemon_pb:with_default(undefined, none)),
     ?_assertEqual(1,pokemon_pb:with_default(undefined, 1)),
     ?_assertEqual(1,pokemon_pb:with_default(1,2)),
     ?_assertEqual(1,pokemon_pb:with_default(1,none))].

pack_test_() ->
    [?_assertEqual([], pokemon_pb:pack(1, optional, undefined, string, [])),
     ?_assertEqual([], pokemon_pb:pack(1, repeated, undefined, string, [])),
     ?_assertEqual(protobuffs:encode(1,1,int32), pokemon_pb:pack(1,required,1,int32,[])),
     ?_assertEqual([protobuffs:encode(1,1,int32),protobuffs:encode(1,2,int32)],pokemon_pb:pack(1,repeated,[1,2],int32,[])),
     ?_assertEqual(protobuffs:encode(1,1,int32),pokemon_pb:pack(1,required,value,pikachu,[]))].
 

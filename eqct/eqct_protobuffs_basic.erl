%% @doc A basic QuickCheck testing module.
-module(eqct_protobuffs_basic).
-compile(export_all). 
-include_lib("eqc/include/eqc.hrl").

%% @spec int32() -> ok
%% @doc Uses QuickCheck to test the int32 encoding and decoding component of
%% protobuffs. It should be noted that this module only tests positives
%% numbers.
int32() -> 
    ?FORALL(
        Xs,
        nat(), 
        {{1, Xs}, <<>>} == protobuffs:decode(list_to_binary(protobuffs:encode(1, Xs, int32)), int32)
    ).

%% @spec string() -> ok
%% @doc Uses QuickCheck to test the string encoding and decoding component
%% of protobuffs.
string() ->
    ?FORALL(
        Xs,
        list(char()), 
        (fun(Var) ->
            {{1, Foo}, <<>>} = protobuffs:decode(list_to_binary(protobuffs:encode(1, Var, string)), string),
            Foo == list_to_binary(Var)
        end)(Xs)
    ).

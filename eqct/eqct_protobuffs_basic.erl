-module(eqct_protobuffs_basic).
-compile(export_all). 
-include_lib("eqc/include/eqc.hrl").

int32() -> 
    ?FORALL(
        Xs,
        nat(), 
        {{1, Xs}, <<>>} == protobuffs:decode(list_to_binary(protobuffs:encode(1, Xs, int32)), int32)
    ).

string() ->
    ?FORALL(
        Xs,
        list(char()), 
        (fun(Var) ->
            {{1, Foo}, <<>>} = protobuffs:decode(list_to_binary(protobuffs:encode(1, Var, string)), string),
            Foo == list_to_binary(Var)
        end)(Xs)
    ).

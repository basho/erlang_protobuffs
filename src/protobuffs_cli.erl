-module(protobuffs_cli).

-export([main/1]).

main ([File]) ->
  protobuffs_compile:generate_source (File);
main (_) ->
  io:format ("usage: ~s <protofile>~n",
             [filename:basename (escript:script_name())]),
  halt (1).

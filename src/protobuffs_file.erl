%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2011 by David AAberg <davabe@hotmail.com>

-module(protobuffs_file).

-export([open/2,path_open/3,close/1,format/3,request/1,compile_forms/2,write_file/2]).

-spec open(file:name(), file:mode()) ->
                  {ok, file:io_device()} |
                  {error, file:posix() | badarg | system_limit}.
open(File, Options) ->
    file:open(File,Options).

-spec path_open(file:name(), file:name(), file:mode()) ->
                       {ok, file:io_device(), file:filename()} |
                       {error, file:posix() | badarg | system_limit}.
path_open(Path, File, Modes) ->
    file:path_open(Path, File, Modes).

-spec close(file:io_device()) -> ok | {error, file:posix() | badarg | terminated}.
close(FileRef) ->
    file:close(FileRef).

-spec format(file:io_device(), io:format(), term()) -> ok.
format(FileRef, FormatString, WriteFields) ->
    io:format(FileRef, FormatString, WriteFields).

-spec request(file:io_device()) -> any.
request(InFile) ->
    io:request(InFile,{get_until,prompt,protobuffs_scanner,token,[1]}).

-spec compile_forms(any(), any()) -> any().
compile_forms(Forms, Options) ->
    compile:forms(Forms, [return] ++ Options).

-spec write_file(file:name(), iodata()) -> ok | {error | file:posix() | badarg | terminated | system_limit}.
write_file(File, Bytes) ->
    file:write_file(File,Bytes).

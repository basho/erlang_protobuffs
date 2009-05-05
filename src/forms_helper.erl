%% Copyright (c) 2009 Jacob Vorreuter
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(forms_helper).

-export([
    init_state/0,
    new_line/0,
    line/0,
    append/1,
    fetch/0,
    io_format/2,
    a/1
]).

%% @hidden
io_format(Format, Arg_Var_Name) ->
    {call,new_line(),
        {remote,line(),{atom,line(),io},{atom,line(),format}},
        [{string,line(),Format}, {cons,line(),{var,line(),Arg_Var_Name},{nil,line()}}]}.

%% @hidden		
a(A) ->
    case erl_parse:abstract(A) of
        {B,_,C} -> {B,line(),C};
        Other -> Other
    end.

%% @hidden
init_state() ->
    Pid1 = spawn_link(fun() -> line_num(1) end),
    put(line_num_pid, Pid1),

    Pid2 = spawn_link(fun() -> forms([]) end),
    put(forms_pid, Pid2),

    ok.

%% @hidden
new_line() ->
    Pid = get(line_num_pid),
    Pid ! {self(), new_line},
    Line_Number1 =
        receive
            {Pid, Line_Number} -> Line_Number
            after 1000 ->
                0
        end,
    Line_Number1.

%% @hidden
line() ->
    Pid = get(line_num_pid),
    Pid ! {self(), line},
    Line_Number1 =
        receive
            {Pid, Line_Number} -> Line_Number
            after 1000 ->
                0
        end,
    Line_Number1.

%% @hidden
append(Forms) ->
    Pid = get(forms_pid),
    Pid ! {self(), append, Forms},
    receive
        {Pid, ok} -> ok
        after 1000 ->
            error
    end,
    ok.

%% @hidden
fetch() ->
    Pid = get(forms_pid),
    Pid ! {self(), fetch},
    Forms1 =
        receive
            {Pid, Forms} -> Forms
            after 1000 ->
                []
        end,
    Forms1.

%% @hidden
line_num(Line_Number) ->
    receive
        {Pid, new_line} ->
            Pid ! {self(), Line_Number+1},
            line_num(Line_Number+1);
        {Pid, line} ->
            Pid ! {self(), Line_Number},
            line_num(Line_Number)
    end.

%% @hidden
forms(Forms) ->
    receive
        {Pid, append, New} ->
            Pid ! {self(), ok},
            forms(lists:reverse([New|lists:reverse(Forms)]));
        {Pid, fetch} ->
            Pid ! {self(), Forms},
            forms(Forms)
    end.

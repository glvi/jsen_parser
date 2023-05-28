%%% jsen_parser - JSON parser in Erlang.
%%% Copyright (C) 2023  GLVI Gesellschaft für Luftverkehrsinformatik mbH, Germany.
%%%
%%% This program is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see
%%% <https://www.gnu.org/licenses/>.
-module(json).
-export([from_file/1, from_string/1]).
-define(NODEBUG, 1). %% Undefine to see ?debug... output
-include_lib("kernel/include/logger.hrl").

from_file(File) ->
    json_parser:parse_and_scan({fun parse_and_scan/1, [File]}).

from_string(String) ->
    parse(json_scanner:string(String)).


parse({ok, Tokens, _}) ->
    json_parser:parse(Tokens);

parse(Other) ->
    ?LOG_ERROR("~tp", [Other]),
    Other.



parse_and_scan(File) ->
    Request = {get_until, unicode, '', json_scanner, token, []},
    File ! {io_request, self(), json_scanner, Request},
    receive
        {io_reply, json_scanner, Data} ->
            scanner_replied(Data);
        Reply ->
            %% io:format("Unexpected IO reply:~tp~n", [Reply]),
            {error, {io, {unexpected, Reply}}, 9999}
    after 1500 ->
            {error, {io, timeout}, 9999}
    end.



scanner_replied({ok, {Token, _Line, Chars} = T, Loc}) ->
    ?LOG_DEBUG("token:{~tp:~ts}~n", [Token, Chars]),
    {ok, [T], Loc};

scanner_replied({ok, {Token, _Line} = T, Loc}) ->
    ?LOG_DEBUG("token:{~tp}~n", [Token]),
    {ok, [T], Loc};

scanner_replied({eof, _Loc} = Eof) ->
    ?LOG_DEBUG("eof:~n"),
    Eof;

scanner_replied({error, ErrorDesc, _Loc} = Error) ->
    ?LOG_ERROR("error:{~tp}~n", [ErrorDesc]),
    Error;

scanner_replied(Reply) ->
    ?LOG_ERROR("Unexpected scanner reply:~tp~n", [Reply]),
    {error, {scanner, {unexpected, Reply}}, 1}.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unit tests below this line

-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).

from_file_test_() ->
    {"json:from_file(\"...\")",
     fun() ->
             FileOpen = file:open("test.json", [binary]),
             ?assertMatch({ok, _}, FileOpen),
             File = element(2, FileOpen),
             FromFile = from_file(File),
             ?assertMatch({ok, _}, FromFile),
             Result = element(2, FromFile),
             ?debugVal(Result)
     end}.

from_string_test_() ->
    {"json:from_string(\"...\")",
     fun () ->
             FromString = from_string("[null, false, -123.456E-7]"),
             ?assertMatch({ok, _}, FromString),
             Result = element(2, FromString),
             ?debugVal(Result)
     end}.

-endif.

%%% Unit tests above this line
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% (End of file)

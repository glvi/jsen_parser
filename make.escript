#! /usr/bin/env escript
%%! -pa /usr/local/kerl/25.1.2/lib/eunit-2.8.1/ebin
%% -*- mode: erlang; coding: utf-8; indent-tabs-mode: nil; -*-
-include_lib("kernel/include/file.hrl").

main(_Args) ->
    {ok, _}     = compile_xrl("json_scanner"),
    {ok, _}     = compile_yrl("json_parser"),
    up_to_date  = make:all(),
    ok          = test({module, json}).

compile_xrl(Base) ->
    {ok, In} = file:read_file_info(Base ++ ".xrl"),
    InTime = In#file_info.ctime,
    OutTime = case file:read_file_info(Base ++ ".erl") of
                  {ok, Out} -> Out#file_info.ctime;
                  _ -> 0 end,
    if OutTime < InTime ->
            io:format("Generate lexer: ~ts~n", [Base]),
            leex:file(Base);
       true ->
            {ok, Base}
    end.

compile_yrl(Base) ->
    {ok, In} = file:read_file_info(Base ++ ".yrl"),
    InTime = In#file_info.ctime,
    OutTime = case file:read_file_info(Base ++ ".erl") of
                  {ok, Out} -> Out#file_info.ctime;
                  _ -> 0 end,
    if OutTime < InTime ->
            io:format("Generate parser: ~ts~n", [Base]),
            yecc:file(Base);
       true ->
            {ok, Base}
    end.

test({module, M}) when is_atom(M) ->
    _           = io:format("Module: ~ts~n", [M]),
    {module, _} = code:load_file(json),
    ok          = eunit:test(json).

#!/usr/bin/env escript
%%! +fnu +A 64 -noshell -sasl errlog_type error -pa ebin
%% -*- mode: erlang; coding: us-ascii; indent-tabs-mode: nil -*-
%% vim: set filetype=erlang fileencoding=utf-8 expandtab sw=4 sts=4:
%% Refer to license at the end of this file.
%%
%% Report CSS identifiers that are not mentioned in any source file.
%% SE, created 12-Dec-2014, in Erlang/OTP 17.
%%
%% Important: .html and .yaws files can only be processed when Yaws
%%            is contained in the path of the Erlang VM, i.e., "-pa".

report_css_idents(InfileWildcards, IncludeDirWildcards) ->
    IncludeDirs = css_idents:list_files(IncludeDirWildcards),
    log("; includes: ~p.~n", [IncludeDirs]),
    Files = css_idents:list_files(InfileWildcards),
    {CssFiles,NonCss} = partition_by_extension(Files, "css"),
    {ErlFiles,NonErl} = partition_by_extension(NonCss, "(erl|hrl)"),
    {YawsFiles,_} = partition_by_extension(NonErl, "(html|htm|yaws)"),
    ErlIdents = read_erl_idents(ErlFiles, IncludeDirs),
    YawsIdents = read_yaws_idents(YawsFiles, IncludeDirs),
    SourceIdents = lists:usort(ErlIdents ++ YawsIdents),
    Source = sets:from_list([Ident || {ident,Ident,_} <- SourceIdents]),
    log("; read ~b identifiers from ~b source files.~n",
        [sets:size(Source), length(ErlFiles) + length(YawsFiles)]),
    CssIdents = read_css_idents(CssFiles, IncludeDirs),
    log("; read ~b identifiers from ~b CSS files.~n",
        [length(CssIdents), length(CssFiles)]),
    UnreferencedCss =
        lists:filter(fun ({ident,Ident,_}) ->
                             not sets:is_element(Ident, Source)
                     end,
                     CssIdents),
    log("; ~b CSS identifiers unreferenced from source~n",
        [length(UnreferencedCss)]),
    case UnreferencedCss =:= [] of
        true  -> ok;
        false ->
            io:fwrite("~ts", [css_idents:print_idents(UnreferencedCss)]),
            erlang:halt(1)
    end.

log(Format, Args) ->
    io:fwrite(standard_error, Format, Args).

read_list(ReadFun, Files) ->
    lists:usort(lists:append([ReadFun(F) || F <- Files])).

read_css_idents(Files, _IncludeDirs) ->
    read_list(
      fun (F) ->
              {ok,Idents} = css_idents:read_ident_literals_css(F),
              Idents
      end,
      Files).

read_erl_idents(Files, IncludeDirs) ->
    read_list(
      fun (F) ->
              {ok,Idents} = css_idents:read_ident_literals_erl(F, IncludeDirs),
              Idents
      end,
      Files).

read_yaws_idents(Files, _IncludeDirs) ->
    read_list(
      fun (F) ->
              {ok,Idents} = css_idents:read_ident_literals_yaws(F),
              Idents
      end,
      Files).

partition_by_extension(Filenames, ExtRe) ->
    {ok,ExtRe1} = re:compile("\\." ++ ExtRe ++ "$", [caseless,no_auto_capture]),
    lists:partition(fun (F) -> re:run(F, ExtRe1, [{capture,none}]) =:= match end,
                    Filenames).

main([]) ->
    io:fwrite(standard_error, "usage: [ -I IncludeWc ... ] FileWc ....~n", []),
    erlang:halt(1);
main(Args) ->
    main(Args, []).

main(["-I",IncludeWc|Args], Acc) ->
    main(Args, [IncludeWc|Acc]);
main(Args, Acc) ->
    report_css_idents(Args, lists:reverse(Acc)).

%% -----------------------------------------------------------------------------
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2014, Sebastian Egner.
%% Copyright (c) 2013-2014, Entelios AG, an EnerNOC company.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% (Source: http://opensource.org/licenses/MIT)

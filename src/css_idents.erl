%% -*- mode: erlang; coding: us-ascii; indent-tabs-mode: nil -*-
%% vim: set filetype=erlang fileencoding=utf-8 expandtab sw=4 sts=4:
%% Refer to license at the end of this file.
%%
%% Extract identifier literals from .erl/.hrl/.html/.yaws-files.
%% SE, created 1-Sep-2013, in Erlang/OTP R16B03.
%%
%% Identifier are represented as binary() containing UTF-8,
%% and are converted to lowercase.

-module(css_idents).

-export([read_ident_literals/3,
         read_ident_literals_erl/2,
         read_ident_literals_yaws/1,
         read_ident_literals_css/1,
         %% NOTYET: read_ident_literals_js/1
         save_idents/2,
         css_idents/1
        ]).

%% Extract all CSS identifiers (class, id) from the files specified
%% by InfileWildcards (a list of arguments to css_util:wildcard/2),
%% using IncludeDirWildcards for resolving -include[_lib].
%% Opts contains options, currently only {quiet,true|false}.
%%    The function returns {ok,[{ident,Ident,[Location,..]},..]},
%% or {error,_,_}. Location is {File,[Line|-1,...]}.
read_ident_literals(InfileWildcards, IncludeDirWildcards, Opts) ->
    IncludeDirs = lists:usort([F || W <- IncludeDirWildcards, F <- wildcard(W)]),
    Infiles = lists:usort([F || W <- InfileWildcards, F <- wildcard(W)]),
    Quiet = proplists:get_bool(quiet, Opts),
    {ok,Idents} =
      lists:foldl(
        fun (_Infile, Error={error,_,_}) ->
                Error;
            (Infile, OkAcc={ok,Acc}) ->
                case not Quiet of
                    true -> io:fwrite("Scanning: ~p~n", [Infile]);
                    false -> ok
                end,
                InfileStr = css_util:to_codepoints(Infile),
                case filename:extension(InfileStr) of
                    Ext when Ext =:= ".erl"; Ext =:= ".hrl" ->
                        case read_ident_literals_erl(InfileStr, IncludeDirs) of
                            {ok,Idents} -> {ok,[Idents|Acc]};
                            Error={error,_,_} -> Error
                        end;
                    Ext when Ext =:= ".html"; Ext =:= ".yaws" ->
                        case read_ident_literals_yaws(InfileStr) of
                            {ok,Idents} -> {ok,[Idents|Acc]};
                            Error={error,_,_} -> Error
                        end;
                    ".css" ->
                        case read_ident_literals_css(InfileStr) of
                            {ok,Idents} -> {ok,[Idents|Acc]};
                            Error={error,_,_} -> Error
                        end;
                    _ ->
                        OkAcc
                end
        end,
        {ok,[]},
        Infiles),
    MergeIdents = merge_idents(Idents),
    case not Quiet of
        true -> io:fwrite("Found ~b identifiers.~n", [length(MergeIdents)]);
        false -> ok
    end,
    {ok,MergeIdents}.

wildcard(Wildcard) ->
    {ok,Files} = css_util:wildcard(Wildcard, "."),
    Files.

save_idents(Outfile, Idents) ->
    Data =
        lists:map(
          fun ({ident,Ident,Locations}) ->
                  [fmt("~ts~n", [Ident]),
                   lists:map(
                     fun ({Filename,Lines}) ->
                             fmt("    ~ts:~ts~n", [filename:basename(Filename),
                                                   lines_to_utf8(Lines)])
                     end,
                     lists:sort([{filename:basename(F),Ls}
                                 || {F,Ls} <- Locations])),
                   fmt("~n", [])]
          end,
          Idents),
    css_util:write_file_utf8(Outfile, Data).

lines_to_utf8(Lines) ->
    css_util:join([fmt("~b", [L]) || L <- Lines], ",").

fmt(Format, Args) ->
    io_lib:format(Format, Args).

merge_idents(Idents) ->
    lists:reverse(
      lists:foldl(
        fun (Entry={ident,Ident,Locations}, Acc=[{ident,Ident1,Locations1}|AccTail]) ->
                case Ident =:= Ident1 of
                    true ->
                        MergedLocations = merge_locations(Locations ++ Locations1),
                        [{ident,Ident1,MergedLocations}|AccTail];
                    false ->
                        [Entry|Acc]
                end;
            (Entry={ident,_,_}, []) ->
                [Entry]
        end,
        [],
        lists:usort(lists:flatten([Idents])))).

merge_locations(Locations) ->
    lists:reverse(
      lists:foldl(
        fun (Location={File,Lines}, Acc=[{File1,Lines1}|AccTail]) ->
                case File =:= File1 of
                    true -> [{File1,lists:usort(Lines ++ Lines1)}|AccTail];
                    false -> [Location|Acc]
                end;
            (Location={_,_}, []) ->
                [Location]
        end,
        [],
        lists:usort(lists:flatten([Locations])))).

%% -- CSS identifier syntax --

%% Recognize CSS identifiers. Returns [binary()], where each entry
%% matches the Ident syntax from css_idents_re().
css_idents(Utf8Iolist) ->
    Utf8 = css_util:to_utf8(Utf8Iolist),
    String = string:to_lower(css_util:to_codepoints(Utf8)),
    RunOpts = [{capture,first,binary},global],
    case re:run(css_util:strip2(String), css_ident_re(), RunOpts) of
        {match,Idents} -> lists:map(fun list_to_binary/1, Idents);
        nomatch -> []
    end.

css_ident_re() ->
    %% Cf. http://www.w3.org/TR/CSS21/grammar.html
    Nonascii = "[\240-\377]",
    Unicode = "\\[0-9a-f]{1,6}(?:\r\n|[ \n\r\t\f])?",
    Escape = "(?:"++Unicode++")|\\[^\n\r\f0-9a-f]",
    Nmchar = "[_a-z0-9-]|(?:"++Nonascii++")|(?:"++Escape++")",
    Nmstart = "[_a-z]|(?:"++Nonascii++")|(?:"++Escape++")",
    Ident = "[-]?(?:"++Nmstart++")(?:"++Nmchar++")*",
    %% Idents = "^[ \t]*("++Ident++")(?:[^_a-z\\\240-\377\-]+("++Ident++"))*$",
    %% Idents = "^[ \t]*("++Ident++")(?:[ \t]+("++Ident++"))*$",
    {ok,Re} = re:compile(Ident, [caseless]),
    Re.

%% -- CSS sources (.css) --

read_ident_literals_css(Infile) ->
    case css_file:read_file(Infile) of
        {ok,Css} ->
            Idents = [{ident,Ident,[{Infile,[-1]}]}
                      || {ident,Str} <- lists:flatten(css(Css)),
                         Ident <- css_idents(Str)],
            {ok,merge_idents(Idents)};
        Error={error,_,_} ->
            Error
    end.

css([H|T]) -> [css(H)|css(T)];
css({'@media',_,RulesetList}) -> css(RulesetList);
css({'@',_,Ruleset}) -> css(Ruleset);
css({ruleset,SelectorList,_}) -> css(SelectorList);
css({selector,Selector}) -> css_selector(Selector);
css(_) -> [].

css_selector([{Combinator,SimpleSelector}|Selector])
  when Combinator =:= ' '; Combinator =:= '>'; Combinator =:= '+' ->
    [css_simple_selector(SimpleSelector)|css_selector(Selector)];
css_selector([]) ->
    [].

css_simple_selector({_Element,SimpleSelector1List}) ->
    lists:map(fun css_simple_selector1/1, SimpleSelector1List).

css_simple_selector1({class,Ident}) -> [{ident,Ident}];
css_simple_selector1({id,Ident}) -> [{ident,Ident}];
css_simple_selector1({attr,Ident}) -> [{ident,Ident}];
css_simple_selector1({attr,Ident,_Rel,_Value}) -> [{ident,Ident}];
css_simple_selector1({':',_}) -> [].

%% --- HTML and Yaws sources (.html, .yaws) ---

read_ident_literals_yaws(Infile) ->
    try
        case yaws_html:parse(Infile) of
            {error,Msg} ->
                {error,yaws_html_parse,[{message,Msg},{infile,Infile}]};
            Ehtml ->
                Idents = [{ident,Ident,[{Infile,[-1]}]}
                          || {ident,Ident} <- lists:flatten(ehtml(Ehtml))],
                {ok,merge_idents(Idents)}
        end
    catch
        {error,Reason,Details} ->
            {error,Reason,[{infile,Infile}|Details]};
        error:{badmatch,{error,enoent}} ->
            {error,enoent,[{infile,Infile}]}
    end.

ehtml(Elements) when is_list(Elements) ->
    lists:map(fun ehtml/1, Elements);
ehtml({erl,Attrs,ErlString}) ->
    [ehtml_attrs(Attrs) | yaws_erl(ErlString)];
ehtml({_Tag,Attrs,Elements}) ->
    [ehtml_attrs(Attrs) | lists:map(fun ehtml/1, [Elements])];
ehtml({erl,ErlString}) ->
    yaws_erl(ErlString);
ehtml({_Tag,Attrs}) ->
    ehtml_attrs(Attrs);
ehtml(_) ->
    [].

ehtml_attrs([{Name,String}|Attrs]) when Name =:= class; Name =:= id ->
    [[{ident,Ident} || Ident <- css_idents(String)] | ehtml_attrs(Attrs)];
ehtml_attrs([_|Attrs]) ->
    ehtml_attrs(Attrs);
ehtml_attrs([]) ->
    [].

yaws_erl(ErlString) ->
    Forms = erl_parse(ErlString),
    Idents = erl_source(_Infile="", Forms),
    [{ident,Ident} || {ident,Ident,_Locations} <- Idents].

erl_parse(String) ->
    case erl_scan:string(String) of
        {ok,Tokens,_EndLocation} ->
            lists:map(
              fun (Tokens1) ->
                      case erl_parse:parse_form(Tokens1) of
                          {ok,AbsForm} -> AbsForm;
                          {error,Details} -> throw({error,erl_parse,[{details,Details}]})
                      end
              end,
              split_by_dot(Tokens));
        {error,Details,_EndLine} ->
            throw({error,erl_scan,[{details,Details}]})
    end.

split_by_dot(Tokens) -> % [Tokens]
    split_by_dot(Tokens, [], []).

split_by_dot([Dot={dot,_}|More], Form, Forms) ->
    split_by_dot(More, [], [lists:reverse([Dot|Form])|Forms]);
split_by_dot([Token|More], Form, Forms) ->
    split_by_dot(More, [Token|Form], Forms);
split_by_dot([], [], Forms) ->
    lists:reverse(Forms);
split_by_dot([], Form, Forms) ->
    [lists:reverse(Form)|Forms].

%% --- Erlang sources (.erl, .hrl) ---

%% Read a list of all identifiers from Infile, using IncludeDirs
%% for resolving -include[_lib] paths. Returns a list of entries
%% of the form {ident,Ident,[{File,[Line,..]},..]}, where Ident
%% is a binary matching css_idents_re().
read_ident_literals_erl(Infile, IncludeDirs) ->
    case epp:parse_file(css_util:to_codepoints(Infile),
                        [css_util:to_codepoints(D) || D <- IncludeDirs],
                        _PredefMacros=[]) of
        {ok,Forms} ->
            {ok,merge_idents(erl_source(Infile, Forms))};
        {error,OpenError} ->
            {error,epp_parse_file,[{reason,OpenError},{infile,Infile}]}
    end.

%% Cf. erts-5.9.2/doc/html/absform.html

erl_source(Infile, Forms) -> % deep_list({ident,Ident,[Location,..]})
    lists:foldl(
      fun ({Type,Line,Data}, Acc)
            when Type =:= string; Type =:= atom ->
              Idents = css_idents(Data),
              Entries = [{ident,Ident,[{Infile,[Line]}]}
                         || Ident <- Idents],
              [Entries|Acc]
      end,
      [],
      module_declaration_or_attributes(Infile, Forms)).

module_declaration_or_attributes(Infile, Forms) ->
    lists:flatten([module_declaration_or_attribute(Infile, F) || F <- Forms]).

module_declaration_or_attribute(_Infile, {attribute,_,_Type,_}) ->
    [];
module_declaration_or_attribute(_Infile, {function,_,_Name,_Arity,Clauses}) ->
    clauses(Clauses);
module_declaration_or_attribute(Infile, {error,Details}) ->
    Msg = io_lib:fwrite("Parse error for ~p. Details:~n~p.~n",
                        [Infile, Details]),
    throw({error,lists:flatten(Msg)});
module_declaration_or_attribute(_Infile, {eof,_}) ->
    [].

clauses(Clauses) ->
    [clause(C) || C <- Clauses].

clause({clause,_,_PatternSeq,_GuardSeq,Body}) -> body(Body).

body(Body) ->
    [expr(E) || E <- Body].

expr({integer,_,_}) -> [];
expr({char,_,_}) -> [];
expr({float,_,_}) -> [];
expr(Literal={string,_Line,_Chars}) -> Literal;
expr(Literal={atom,_Line,_Atom}) -> Literal;
expr({match,_,_Pattern,Expr}) -> expr(Expr);
expr({var,_,_}) -> [];
expr({tuple,_,Exprs}) -> [expr(E) || E <- Exprs];
expr({nil,_}) -> [];
expr({cons,_,HeadExpr,TailExpr}) -> [expr(HeadExpr), expr(TailExpr)];
expr({op,_,_Op,E1,E2}) -> [expr(E1), expr(E2)];
expr({op,_,_Op,E}) -> expr(E);
expr({bin,_,BinElements}) -> [bin_element(BE) || BE <- BinElements];
expr({record,_,_Name,Fields}) -> [record_field(RF) || RF <- Fields];
expr({record,_,Expr,_Name,Fields}) -> [expr(Expr),[record_field(RF) || RF <- Fields]];
expr({record_index,_,_Name,_Field}) -> [];
expr({record_field,_,Expr,_Name,_Field}) -> expr(Expr);
expr({'catch',_,Expr}) -> expr(Expr);
expr({call,_,{remote,_,_Mod,_Fun},ArgExprs}) -> [expr(E) || E <- ArgExprs];
expr({call,_,_FunExpr,ArgExprs}) -> [expr(E) || E <- ArgExprs];
expr({lc,_,Expr,WExprs}) -> [expr(Expr),[generate(W) || W <- WExprs]];
expr({bc,_,Expr,WExprs}) -> [expr(Expr),[generate(W) || W <- WExprs]];
expr({block,_,Body}) -> body(Body);
expr({'if',_,Clauses}) -> [clause(C) || C <- Clauses];
expr({'case',_,Expr,Clauses}) -> [expr(Expr),[clause(C) || C <- Clauses]];
expr({'try',_,Body,Cases,Catches,AfterBody}) ->
    [body(Body), clauses(Cases), clauses(Catches), body(AfterBody)];
expr({'receive',_,Clauses}) -> clauses(Clauses);
expr({'receive',_,Clauses,_After,Body}) -> [clauses(Clauses), body(Body)];
expr({'fun',_,{function,_,_}}) -> [];
expr({'fun',_,{function,_,_,_}}) -> [];
expr({'fun',_,{clauses,Clauses}}) -> clauses(Clauses);
expr({'query',_,Lc}) -> expr(Lc);
expr({record_field,_,Expr,_Field}) -> expr(Expr).

bin_element({bin_element,_,Value,_Size,_TSL}) -> expr(Value).

record_field({record_field,_,_Field,Expr}) -> expr(Expr).

generate({generate,_,_Pattern,Expr}) -> expr(Expr);
generate({b_generate,_,_Pattern,Expr}) -> expr(Expr);
generate(FilterExpr) -> expr(FilterExpr).

%% ------------------------------------------------------------------------------
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

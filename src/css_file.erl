%% -*- mode: erlang; coding: us-ascii; indent-tabs-mode: nil -*-
%% vim: set filetype=erlang fileencoding=utf-8 expandtab sw=4 sts=4:
%% Refer to license at the end of this file.
%%
%% I/O for CSS files.
%% SE, created 9-Sep-2013, Erlang/OTP R16B01.
%%
%% We do not (yet?) define types and structures for CSS terms.
%% Use the source of write_file/2 and friends for now.

-module(css_file).

-export([read_file/1,
         write_file/2
        ]).

%% -- reading CSS --

%% Read a CSS file. Returns {ok,Css}|{error,_,_}, where Css is a
%% data structure representing the CSS expressions.
read_file(Filename) ->
    case scan_file(Filename) of
        {ok,[]} ->
            {ok,[]};
        {ok,Tokens} ->
            case css_yecc:parse(Tokens) of
                OkCss={ok,_} ->
                    OkCss;
                {error,{Line,css_yecc,Msg}} ->
                    {error,css_syntax_yecc,
                     [{infile,Filename},
                      {line,Line},
                      {reason,lists:flatten(Msg)}]}
            end;
        Error={error,_,_} ->
            Error
    end.

%% Read a file and run Leex. Returns {ok,Tokens}|{error,_,_}.
%% This function is mainly used for read_file/1, but is occasionally
%% useful for debugging the grammar.
scan_file(Filename) ->
    case css_util:read_file(Filename) of
        {ok,Bytes} ->
            Input = css_util:to_codepoints(Bytes),
            case css_leex:string(Input) of
                {ok,Tokens,_EndLine} ->
                    {ok,Tokens};
                {error,{Line,css_leex,{Reason,Details}},_EndLine} ->
                    {error,css_syntax_leex,
                     [{reason,Reason},
                      {infile,Filename},
                      {line,Line},
                      {reason,lists:flatten(Details)}]}
            end;
        Error={error,_,_} ->
            Error
    end.

%% -- writing CSS --

%% Write a CSS structure into Outfile. Returns ok|{error,_,_}.
%%    The output file is then read back and compared to Css in order
%% to detect problems in the CSS reader/writer as early as possible.
%% This has been proven worth the milliseconds that it takes.
write_file(Outfile, Css) ->
    case css_util:write_file_utf8(Outfile, wr_entries(Css)) of
        ok -> check_readback(Css, Outfile);
        Error={error,_,_} -> Error
    end.

check_readback(Css, Outfile) -> % ok|{error,_,_}
    case read_file(Outfile) of
        {ok,CssReadback} ->
            case css_util:check_diff(Css, CssReadback) of
                ok -> ok;
                {error,Reason,Details} ->
                    {error,css_readback_failed,
                     [{outfile,Outfile},{reason,Reason}|Details]}
            end;
        {error,Reason,Details} ->
            {error,css_readback_failed,
             [{outfile,Outfile},{reason,Reason}|Details]}
    end.

wr_entries(Entries) when is_list(Entries) ->
    join_map(fun wr_entry/1, Entries, "\n\n").

wr_entry({'@charset',Charset}) ->
    ["@charset ", wr_string(Charset), ";"];
wr_entry({'@import',StringOrUri,[]}) ->
    ["@import ", wr_string_or_uri(StringOrUri), ";"];
wr_entry({'@import',StringOrUri,MediaQueryList=[_|_]}) ->
    ["@import ",
     wr_string_or_uri(StringOrUri), " ",
     wr_media_query_list(MediaQueryList), ";"];
wr_entry({'@media',MediaQueryList,RulesetList}) ->
    ["@media ",
     wr_media_query_list(MediaQueryList), " {", wr_nl(1),
     wr_ruleset_list(RulesetList, 1), wr_nl(0),
     "}"];
wr_entry({'@R',Keyword,Ruleset}) ->
    ["@", wr_ident(Keyword), " ",
     wr_ruleset(Ruleset, 0)];
wr_entry({'@R',Keyword,{function,Name,Expr},RulesetList}) ->
    ["@", wr_ident(Keyword), " ",
     wr_function(Name, Expr), " ",
     wr_ruleset_list(RulesetList, 1)];
wr_entry({'@R',Keyword,{function,Name,Expr},{'/*validator:*/',Pragma},RulesetList}) ->
    ["@", wr_ident(Keyword), " ",
     wr_function(Name, Expr), " ",
     wr_validator(Pragma), wr_nl(0),
     "{", wr_nl(1),
     wr_ruleset_list(RulesetList, 1), wr_nl(0),
     "}"];
wr_entry({'@D',Keyword,DeclarationList}) ->
    ["@", wr_ident(Keyword), " {", wr_nl(1),
     wr_declaration_list(DeclarationList, 1), wr_nl(0),
     "}"];
wr_entry({'@D',Keyword,{function,Name,Expr},DeclarationList}) ->
    ["@", wr_ident(Keyword), " ",
     wr_function(Name, Expr), " {", wr_nl(1),
     wr_declaration_list(DeclarationList, 1), wr_nl(0),
     "}"];
wr_entry({'@D',Keyword,{function,Name,Expr},{'/*validator:*/',Pragma},DeclarationList}) ->
    ["@", wr_ident(Keyword), " ",
     wr_function(Name, Expr), " ",
     wr_validator(Pragma), wr_nl(0),
     "{", wr_nl(1),
     wr_declaration_list(DeclarationList, 1), wr_nl(0),
     "}"];
wr_entry({'@:',Keyword,Expr}) ->
    ["@", wr_ident(Keyword), ": ", wr_expr(Expr), ";"];
wr_entry(Ruleset={ruleset,_,_}) ->
    wr_ruleset(Ruleset, 0).

%wr_media_list(MediaList) ->
%    join_map(fun wr_ident/1, MediaList, ", ").

wr_media_query_list(MediaQueryList) ->
    join_map(fun wr_media_query/1, MediaQueryList, ", ").

wr_media_query({media_query,MediaType,Expressions}) ->
    join([wr_media_type(MediaType),
          join_map(fun wr_expression/1, Expressions, " and ")],
         " and ").

wr_media_type(any) ->
    "";
wr_media_type({'only',Ident}) ->
    ["only ", wr_ident(Ident)];
wr_media_type({'not',Ident}) ->
    ["not ", wr_ident(Ident)];
wr_media_type(Ident) ->
    wr_ident(Ident).

wr_expression({':',MediaFeature,Expr}) ->
    ["(", wr_media_feature(MediaFeature),": ",wr_expr(Expr), ")"];
wr_expression(Expr) ->
    ["(", wr_expr(Expr), ")"].

wr_media_feature(MediaFeature) ->
    wr_ident(MediaFeature).

wr_ruleset_list(RulesetList, Indent) ->
    join_map(fun (R) -> wr_ruleset(R, Indent) end,
             RulesetList,
             wr_nl(0) ++ wr_nl(Indent)).

wr_ruleset({ruleset,SelectorList,[]}, _Indent) ->
    [wr_selector_list(SelectorList), " { }"];
wr_ruleset({ruleset,SelectorList,DeclarationList=[_|_]}, Indent) ->
    [wr_selector_list(SelectorList),
     " {", wr_nl(Indent + 1),
     wr_declaration_list(DeclarationList, Indent + 1),
     wr_nl(Indent), "}"].

wr_selector_list(SelectorList) ->
    case lists:reverse(SelectorList) of
        [{'/*validator:*/',Pragma}|RevSelectors] ->
            S = join_map(fun wr_selector/1, lists:reverse(RevSelectors), ", "),
            [S,wr_validator(Pragma)];
        _ ->
            join_map(fun wr_selector/1, SelectorList, ", ")
    end.

wr_selector({selector,[{' ',SS}|SSs]}) ->
    join([wr_simple_selector(SS)] ++
             lists:map(fun ({' ',SS1}) ->
                               [" ", wr_simple_selector(SS1)];
                           ({Combinator,SS1}) ->
                               [wr_atom(Combinator), " ",
                                wr_simple_selector(SS1)]
                       end,
                       SSs),
         " ").

wr_simple_selector({'*',SS1s}) ->
    case css_util:to_utf8(wr_simple_selector1s(SS1s)) of
        <<>> -> <<"*">>;
        Other -> Other
    end;
wr_simple_selector({Element,SS1s}) ->
    join([wr_ident(Element), wr_simple_selector1s(SS1s)], "").

wr_simple_selector1s(SimpleSelector1s) ->
    join_map(fun wr_simple_selector1/1, SimpleSelector1s, "").

wr_simple_selector1({class,Ident}) ->
    [".", wr_ident(Ident)];
wr_simple_selector1({id,Ident}) ->
    ["#", wr_ident(Ident)];
wr_simple_selector1({attr,Ident}) ->
    ["[", wr_ident(Ident), "]"];
wr_simple_selector1({attr,Ident,Rel,{string,String}}) ->
    ["[", wr_ident(Ident), wr_atom(Rel), wr_string(String), "]"];
wr_simple_selector1({attr,Ident,Rel,{ident,Rhs}}) ->
    ["[", wr_ident(Ident), wr_atom(Rel), wr_ident(Rhs), "]"];
wr_simple_selector1({':',{ident,Ident}}) ->
    [":", wr_ident(Ident)];
wr_simple_selector1({'::',{ident,Ident}}) ->
    ["::", wr_ident(Ident)];
wr_simple_selector1({':',{function,Name,Expr}}) ->
    [":", wr_function(Name, Expr)];
wr_simple_selector1({'::',{function,Name,Expr}}) ->
    ["::", wr_function(Name, Expr)];
wr_simple_selector1({'#',Class}) ->
    ["#", wr_simple_selector1(Class)].

wr_declaration_list(DeclarationList, Indent) ->
    join(wr_declaration_list1(DeclarationList), wr_nl(Indent)).

wr_declaration_list1([D,{'/*validator:*/',Pragma}|Ds]) ->
    [[wr_declaration(D), wr_validator(Pragma)]|wr_declaration_list1(Ds)];
wr_declaration_list1([D|Ds]) ->
    [wr_declaration(D)|wr_declaration_list1(Ds)];
wr_declaration_list1([]) ->
    [].

wr_declaration({':',Property,Expr}) ->
    [wr_property(Property), ": ", wr_expr(Expr), ";"];
wr_declaration({':!important',Property,Expr}) ->
    [wr_property(Property), ": ", wr_expr(Expr), " !important;"];
wr_declaration({'{}',Term={Num,Unit},DeclarationList})
  when is_number(Num), is_atom(Unit) ->
    [wr_term(Term), " { ",
     join_map(fun wr_declaration/1,
              DeclarationList, " "), " }"];
wr_declaration({'{}',Property,DeclarationList}) ->
    [wr_ident(Property), " { ",
     join_map(fun wr_declaration/1,
              DeclarationList, " "), " }"].

wr_property({'*',Ident}) ->
    ["*",wr_ident(Ident)];
wr_property(Ident) ->
    wr_ident(Ident).

wr_validator(Pragma) ->
    [" /* validator: ", Pragma, " */"].

wr_expr({Op,Args}) when Op =:= ','; Op =:= ' '; Op =:= '/' ->
    join_map(fun wr_expr/1, Args, wr_atom(Op));
wr_expr(Term) ->
    wr_term(Term).

wr_term({Num,Unit}) when is_number(Num), is_atom(Unit) ->
    [number_to_string(Num), wr_atom(Unit)];
wr_term(Num) when is_number(Num) ->
    number_to_string(Num);
wr_term({ident,Ident}) ->
    wr_ident(Ident);
wr_term({string,String}) ->
    wr_string(String);
wr_term({uri,Uri}) ->
    wr_uri(Uri);
wr_term({hexcolor,Hexcolor}) ->
    ["#", wr_escaped(Hexcolor)];
wr_term({function,Name,Args}) ->
    wr_function(Name, Args);
wr_term({':',Ident}) ->
    [":", wr_ident(Ident)];
wr_term({'.',Ident}) ->
    [".", wr_ident(Ident)];
wr_term({'@',AtSym}) ->
    ["@", wr_ident(AtSym)];
wr_term({'=',Lhs,Rhs}) ->
    [wr_expr(Lhs), "=", wr_expr(Rhs)];
wr_term({lua,String}) ->
    ["<?lua ", wr_escaped(String), " ?>"].

wr_function(Name, []) ->
    [wr_ident(Name), "()"];
wr_function(Name, Expr) ->
    [wr_ident(Name), "(", wr_expr(Expr), ")"].

wr_atom(Atom) ->
    atom_to_binary(Atom, utf8).

wr_string_or_uri({string,String}) ->
    wr_string(String);
wr_string_or_uri({uri,Uri}) ->
    wr_uri(Uri).

wr_uri({string,String}) ->
    ["url(", wr_string(String), ")"];
wr_uri({url,Url}) ->
    ["url(", wr_escaped(Url), ")"].

wr_string(String) ->
    ["\"", wr_escaped(String), "\""].

wr_ident({':',Namespace,Ident}) ->
    [wr_escaped(Namespace), ":", wr_ident(Ident)];
wr_ident({'.',Ident1,Ident2}) ->
    [wr_ident(Ident1), ".", wr_ident(Ident2)];
wr_ident({ident,Ident}) ->
    wr_ident(Ident);
wr_ident(Ident) ->
    wr_escaped(Ident).

wr_escaped(Str) ->
    lists:map(fun wr_esc/1, css_util:to_codepoints(Str)).

wr_esc( 9) -> "\\t";
wr_esc(10) -> "\\n";
wr_esc(12) -> "\\f";
wr_esc(13) -> "\\r";
wr_esc(34) -> "\\\"";
wr_esc(92) -> "\\\\";
wr_esc(C) when 32 =< C, C =< 126 -> C;
wr_esc(C) -> io_lib:format("\\~.16B ", [C]).

wr_nl(Indent) ->
    "\n" ++ [32 || _ <- lists:seq(1, 4*Indent)].

join_map(Fun, List, Sep) ->
    join(lists:map(Fun, List), Sep).

%% The paranoid way to convert a number() into a string() such that
%% its bit pattern can be reconstructed exactly upon reading.
number_to_string(N) when is_integer(N) ->
    integer_to_binary(N);
number_to_string(X) when is_float(X) ->
    S1 = lists:flatten(io_lib:format("~w", [X])), % short, if it works
    X1 = list_to_float(S1),
    case X1 =:= X of
        true  -> S1;
        false ->
            S2 = float_to_list(X),
            X2 = list_to_float(S2),
            case X2 =:= X of
                true  -> S2;
                false -> exit({unprintable_float,X})
            end
    end.

%% Abbreviations.

join(Args, Separator) ->
    css_util:join(Args, Separator).

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

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

-export([rewrite_file/1,
         rewrite_file/2,
         read_file/1,
         write_file/2,
         scan_file/1
        ]).

%% -- rewrite CSS --

%% Transcode CSS from Infile to some Outfile.
%% Returns {ok,Outfile}|{error,_,_}.
rewrite_file(Infile) ->
    Outfile = css_util:to_codepoints(fmt("~ts-rewrite.css",
                                         [filename:rootname(Infile)])),
    case rewrite_file(Infile, Outfile) of
        ok -> {ok,Outfile};
        Error={error,_,_} -> Error
    end.

%% Transcode CSS from Infile to Outfile. Returns ok|{error,_,_}.
rewrite_file(Infile, Outfile) ->
    case read_file(Infile) of
        {ok,Css} -> write_file(Outfile, Css);
        Error={error,_,_} -> Error
    end.

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
                     [{outfile,Outfile},{css,Css},{reason,Reason}|Details]}
            end;
        {error,Reason,Details} ->
            {error,css_readback_failed,
             [{outfile,Outfile},{css,Css},{reason,Reason}|Details]}
    end.

wr_entries(Entries) when is_list(Entries) ->
    join_map(fun wr_entry/1, Entries, "\n\n").

wr_entry({'@charset',Charset}) ->
    fmt("@charset ~ts;", [wr_string(Charset)]);
wr_entry({'@import',StringOrUri,[]}) ->
    fmt("@import ~ts;", [wr_string_or_uri(StringOrUri)]);
wr_entry({'@import',StringOrUri,MediaList=[_|_]}) ->
    fmt("@import ~ts ~ts;", [wr_string_or_uri(StringOrUri), wr_media_list(MediaList)]);
wr_entry({'@media',MediaList,RulesetList}) ->
    fmt("@media ~ts {~n~ts~n}", [wr_media_list(MediaList), wr_ruleset_list(RulesetList)]);
wr_entry({'@',Keyword,Ruleset}) ->
    fmt("@~ts ~ts", [wr_ident(Keyword), wr_ruleset(Ruleset)]);
wr_entry({'@',Keyword,{function,Name,Expr},RulesetList}) ->
    fmt("@~ts ~ts ~ts", [wr_ident(Keyword),
                         wr_function(Name, Expr),
                         wr_ruleset_list(RulesetList)]);
wr_entry({'@',Keyword,{function,Name,Expr},{'/*validator:*/',Pragma},RulesetList}) ->
    fmt("@~ts ~ts ~ts~n{~n~ts~n}", % NOTYET: pretty-print
        [wr_ident(Keyword),
         wr_function(Name, Expr),
         wr_validator(Pragma),
         wr_ruleset_list(RulesetList)]);
wr_entry(Ruleset={ruleset,_,_}) ->
    wr_ruleset(Ruleset).

wr_media_list(MediaList) ->
    join_map(fun wr_ident/1, MediaList, ", ").

wr_ruleset_list(RulesetList) ->
    join_map(fun wr_ruleset/1, RulesetList, "\n").

wr_ruleset({ruleset,SelectorList,[]}) ->
    fmt("~ts { }", [wr_selector_list(SelectorList)]);
wr_ruleset({ruleset,SelectorList,DeclarationList=[_|_]}) ->
    fmt("~ts {\n    ~ts\n}", [wr_selector_list(SelectorList),
                              wr_declaration_list(DeclarationList)]).

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
                               fmt(" ~ts", [wr_simple_selector(SS1)]);
                           ({Combinator,SS1}) ->
                               fmt("~ts ~ts", [wr_atom(Combinator),
                                               wr_simple_selector(SS1)])
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
    fmt(".~ts", [wr_ident(Ident)]);
wr_simple_selector1({id,Ident}) ->
    fmt("#~ts", [wr_ident(Ident)]);
wr_simple_selector1({attr,Ident}) ->
    fmt("[~ts]", [wr_ident(Ident)]);
wr_simple_selector1({attr,Ident,Rel,{string,String}}) ->
    fmt("[~ts~ts~ts]", [wr_ident(Ident), wr_atom(Rel), wr_string(String)]);
wr_simple_selector1({attr,Ident,Rel,Rhs}) ->
    fmt("[~ts~ts~ts]", [wr_ident(Ident), wr_atom(Rel), wr_ident(Rhs)]);
wr_simple_selector1({':',{ident,Ident}}) ->
    fmt(":~ts", [wr_ident(Ident)]);
wr_simple_selector1({':',{function,Name,Expr}}) ->
    fmt(":~ts", [wr_function(Name, Expr)]).

wr_declaration_list(DeclarationList) ->
    join(wr_declaration_list1(DeclarationList), "\n    ").

wr_declaration_list1([D,{'/*validator:*/',Pragma}|Ds]) ->
    S = fmt("~ts~ts", [wr_declaration(D), wr_validator(Pragma)]),
    [S|wr_declaration_list1(Ds)];
wr_declaration_list1([D|Ds]) ->
    [wr_declaration(D)|wr_declaration_list1(Ds)];
wr_declaration_list1([]) ->
    [].

wr_declaration({':',Property,Expr}) ->
    fmt("~ts: ~ts;", [wr_ident(Property), wr_expr(Expr)]);
wr_declaration({':!important',Property,Expr}) ->
    fmt("~ts: ~ts !important;", [wr_ident(Property), wr_expr(Expr)]);
wr_declaration({'{}',Term={Num,Unit},DeclarationList})
  when is_number(Num), is_atom(Unit) ->
    fmt("~ts { ~ts }", [wr_term(Term),
                        join_map(fun wr_declaration/1,
                                 DeclarationList, " ")]);
wr_declaration({'{}',Property,DeclarationList}) ->
    fmt("~ts { ~ts }", [wr_ident(Property),
                        join_map(fun wr_declaration/1,
                                 DeclarationList, " ")]).

wr_validator(Pragma) ->
    fmt(" /* validator: ~ts */", [Pragma]).

wr_expr({Op,Args}) when Op =:= ','; Op =:= ' '; Op =:= '/' ->
    join_map(fun wr_expr/1, Args, wr_atom(Op));
wr_expr(Term) ->
    wr_term(Term).

wr_term({Num,Unit}) when is_number(Num), is_atom(Unit) ->
    fmt("~ts~ts", [number_to_string(Num), wr_atom(Unit)]);
wr_term(Num) when is_number(Num) ->
    number_to_string(Num);
wr_term({ident,Ident}) ->
    wr_ident(Ident);
wr_term({string,String}) ->
    wr_string(String);
wr_term({uri,Uri}) ->
    wr_uri(Uri);
wr_term({hexcolor,Hexcolor}) ->
    fmt("#~ts", [wr_escaped(Hexcolor)]);
wr_term({function,Name,Args}) ->
    wr_function(Name, Args).

wr_function(Name, []) ->
    fmt("~ts()", [wr_ident(Name)]);
wr_function(Name, Expr) ->
    fmt("~ts(~ts)", [wr_ident(Name), wr_expr(Expr)]).

wr_atom(Atom) ->
    atom_to_binary(Atom, utf8).

wr_string_or_uri({string,String}) ->
    wr_string(String);
wr_string_or_uri({uri,Uri}) ->
    wr_uri(Uri).

wr_uri({string,String}) ->
    fmt("url(~ts)", [wr_string(String)]);
wr_uri({url,Url}) ->
    fmt("url(~ts)", [wr_escaped(Url)]).

wr_string(String) ->
    fmt("\"~ts\"", [wr_escaped(String)]).

wr_ident(Ident) ->
    wr_escaped(Ident).

wr_escaped(Str) ->
    wr_esc(css_util:to_codepoints(Str), <<>>).

wr_esc([ 9|T], Acc) ->  wr_esc(T, <<Acc/binary,"\\t">>);
wr_esc([10|T], Acc) ->  wr_esc(T, <<Acc/binary,"\\n">>);
wr_esc([12|T], Acc) ->  wr_esc(T, <<Acc/binary,"\\f">>);
wr_esc([13|T], Acc) ->  wr_esc(T, <<Acc/binary,"\\r">>);
wr_esc([34|T], Acc) ->  wr_esc(T, <<Acc/binary,"\\\"">>);
wr_esc([92|T], Acc) ->  wr_esc(T, <<Acc/binary,"\\\\">>);
wr_esc([H|T], Acc) when 32 =< H, H =< 126 ->
    case 32 =< H andalso H =< 126 of
        true  -> wr_esc(T, <<Acc/binary,H>>);
        false -> wr_esc(T, <<Acc/binary,(wr_hex(H, T))/binary>>)
    end;
wr_esc([], Acc) ->
    Acc.

wr_hex(N, Tail) ->
    NStr = string:to_lower(integer_to_list(N, 16)),
    case re:run(Tail, "^[0-9A-Fa-f]", [{capture,none}]) of
        match   -> ["\\", NStr, " "];
        nomatch -> ["\\", NStr]
    end.

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

fmt(Format, Args) ->
    io_lib:format(Format, Args).

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

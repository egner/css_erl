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
    Outfile = uni:fmt("~ts-rewrite.css",
                      [uni:to_utf8(filename:rootname(Infile))]),
    case rewrite_file(Infile, Outfile) of
        ok -> {ok,Outfile};
        Error={error,_,_} -> Error
    end.

%% Transcode CSS from Infile to Outfile. Returns ok|{error,_,_}.
rewrite_file(Infile, Outfile) ->
    case read_file(Infile) of
        {ok,Css} ->
            case write_file(Outfile, Css) of
                ok -> check_readback(Infile, Css, Outfile);
                Error={error,_,_} -> Error
            end;
        Error={error,_,_} -> Error
    end.

check_readback(Infile, Css, Outfile) -> % ok|{error,_,_}
    case read_file(Outfile) of
        {ok,CssReadback} ->
            case utils:check_diff(Css, CssReadback, []) of
                ok -> ok;
                {error,Reason,Details} ->
                    {error,css_readback_failed,
                     [{infile,Infile},
                      {outfile,Outfile},
                      {reason,Reason}|Details]}
            end;
        {error,Reason,Details} ->
            {error,css_readback_failed,
             [{infile,Infile},
              {outfile,Outfile},
              {reason,Reason}|Details]}
    end.

%% -- reading CSS --

%% Read a CSS file. Returns {ok,Css}|{error,_,_}, where Css is a
%% data structure representing the CSS expressions.
read_file(Filename) ->
    case scan_file(Filename) of
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
    case zfile:read_file(Filename) of
        {ok,Bytes} ->
            Input = uni:to_codepoints(Bytes),
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
write_file(Outfile, Css) ->
    zfile:write_file(Outfile, wr_entries(Css)).

wr_entries(Entries) when is_list(Entries) ->
    join_map(fun wr_entry/1, Entries, "\n\n").

wr_entry({'@charset',Charset}) ->
    uni:fmt("@charset ~ts;", [wr_string(Charset)]);
wr_entry({'@import',StringOrUri,[]}) ->
    uni:fmt("@import ~ts;", [wr_string_or_uri(StringOrUri)]);
wr_entry({'@import',StringOrUri,MediaList=[_|_]}) ->
    uni:fmt("@import ~ts ~ts;",
            [wr_string_or_uri(StringOrUri),
             wr_media_list(MediaList)]);
wr_entry({'@media',MediaList,RulesetList}) ->
    uni:fmt("@media ~ts {~n~ts~n}", [wr_media_list(MediaList),
                                     wr_ruleset_list(RulesetList)]);
wr_entry({'@',Keyword,Ruleset}) ->
    uni:fmt("@~ts ~ts", [wr_ident(Keyword),
                         wr_ruleset(Ruleset)]);
wr_entry({'@',Keyword,{function,Name,Expr},RulesetList}) ->
    uni:fmt("@~ts ~ts ~ts", [wr_ident(Keyword),
                             wr_function(Name, Expr),
                             wr_ruleset_list(RulesetList)]);
wr_entry({'@',Keyword,{function,Name,Expr},{'/*validator:*/',Pragma},RulesetList}) ->
    uni:fmt("@~ts ~ts ~ts~n{~n~ts~n}", % FIXME: pretty-print
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
    uni:fmt("~ts { }", [wr_selector_list(SelectorList)]);
wr_ruleset({ruleset,SelectorList,DeclarationList=[_|_]}) ->
    uni:fmt("~ts {\n    ~ts\n}", [wr_selector_list(SelectorList),
                                  wr_declaration_list(DeclarationList)]).

wr_selector_list(SelectorList) ->
    case lists:reverse(SelectorList) of
        [{'/*validator:*/',Pragma}|RevSelectors] ->
            S = join_map(fun wr_selector/1, lists:reverse(RevSelectors), ", "),
            <<S/binary,(wr_validator(Pragma))/binary>>;
        _ ->
            join_map(fun wr_selector/1, SelectorList, ", ")
    end.

wr_selector({selector,[{' ',SS}|SSs]}) ->
    uni:join([wr_simple_selector(SS)] ++
                 lists:map(fun ({' ',SS1}) ->
                                   uni:fmt(" ~ts",
                                           [wr_simple_selector(SS1)]);
                               ({Combinator,SS1}) ->
                                   uni:fmt("~ts ~ts",
                                           [wr_atom(Combinator),
                                            wr_simple_selector(SS1)])
                           end,
                           SSs),
             " ").

wr_simple_selector({'*',SS1s}) ->
    case wr_simple_selector1s(SS1s) of
        <<>> -> <<"*">>;
        Other -> Other
    end;
wr_simple_selector({Element,SS1s}) ->
    uni:join([wr_ident(Element), wr_simple_selector1s(SS1s)], "").

wr_simple_selector1s(SimpleSelector1s) ->
    join_map(fun wr_simple_selector1/1, SimpleSelector1s, "").

wr_simple_selector1({class,Ident}) ->
    uni:fmt(".~ts", [wr_ident(Ident)]);
wr_simple_selector1({id,Ident}) ->
    uni:fmt("#~ts", [wr_ident(Ident)]);
wr_simple_selector1({attr,Ident}) ->
    uni:fmt("[~ts]", [wr_ident(Ident)]);
wr_simple_selector1({attr,Ident,Rel,{string,String}}) ->
    uni:fmt("[~ts~ts~ts]", [wr_ident(Ident),
                            wr_atom(Rel),
                            wr_string(String)]);
wr_simple_selector1({attr,Ident,Rel,Rhs}) ->
    uni:fmt("[~ts~ts~ts]", [wr_ident(Ident),
                            wr_atom(Rel),
                            wr_ident(Rhs)]);
wr_simple_selector1({':',{ident,Ident}}) ->
    uni:fmt(":~ts", [wr_ident(Ident)]);
wr_simple_selector1({':',{function,Name,Expr}}) ->
    uni:fmt(":~ts", [wr_function(Name, Expr)]).

wr_declaration_list(DeclarationList) ->
    uni:join(wr_declaration_list1(DeclarationList), "\n    ").

wr_declaration_list1([D,{'/*validator:*/',Pragma}|Ds]) ->
    S = uni:fmt("~ts~ts", [wr_declaration(D), wr_validator(Pragma)]),
    [S|wr_declaration_list1(Ds)];
wr_declaration_list1([D|Ds]) ->
    [wr_declaration(D)|wr_declaration_list1(Ds)];
wr_declaration_list1([]) ->
    [].

wr_declaration({':',Property,Expr}) ->
    uni:fmt("~ts: ~ts;",
            [wr_ident(Property),
             wr_expr(Expr)]);
wr_declaration({':!important',Property,Expr}) ->
    uni:fmt("~ts: ~ts !important;",
            [wr_ident(Property),
             wr_expr(Expr)]);
wr_declaration({'{}',Term={Num,Unit},DeclarationList})
  when is_number(Num), is_atom(Unit) ->
    uni:fmt("~ts { ~ts }",
            [wr_term(Term),
             join_map(fun wr_declaration/1, DeclarationList, " ")]);
wr_declaration({'{}',Property,DeclarationList}) ->
    uni:fmt("~ts { ~ts }",
            [wr_ident(Property),
             join_map(fun wr_declaration/1, DeclarationList, " ")]).

wr_validator(Pragma) ->
    uni:fmt(" /* validator: ~ts */", [Pragma]).

wr_expr({Op,Args}) when Op =:= ','; Op =:= ' '; Op =:= '/' ->
    join_map(fun wr_expr/1, Args, wr_atom(Op));
wr_expr(Term) ->
    wr_term(Term).

wr_term({Num,Unit}) when is_number(Num), is_atom(Unit) ->
    uni:fmt("~ts~ts", [number:to_utf8_exact(Num),
                       wr_atom(Unit)]);
wr_term(Num) when is_number(Num) ->
    number:to_utf8_exact(Num);
wr_term({ident,Ident}) ->
    wr_ident(Ident);
wr_term({string,String}) ->
    wr_string(String);
wr_term({uri,Uri}) ->
    wr_uri(Uri);
wr_term({hexcolor,Hexcolor}) ->
    uni:fmt("#~ts", [wr_escaped(Hexcolor)]);
wr_term({function,Name,Args}) ->
    wr_function(Name, Args).

wr_function(Name, []) ->
    uni:fmt("~ts()", [wr_ident(Name)]);
wr_function(Name, Expr) ->
    uni:fmt("~ts(~ts)", [wr_ident(Name), wr_expr(Expr)]).

wr_atom(Atom) ->
    atom_to_binary(Atom, utf8).

wr_string_or_uri({string,String}) ->
    wr_string(String);
wr_string_or_uri({uri,Uri}) ->
    wr_uri(Uri).

wr_uri({string,String}) ->
    uni:fmt("url(~ts)", [wr_string(String)]);
wr_uri({url,Url}) ->
    uni:fmt("url(~ts)", [wr_escaped(Url)]).

wr_string(String) ->
    uni:fmt("\"~ts\"", [wr_escaped(String)]).

wr_ident(Ident) ->
    wr_escaped(Ident).

wr_escaped(Str) ->
    wr_esc(uni:to_codepoints(Str), <<>>).

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
    NStr = uni:lowercase(integer_to_binary(N, 16)),
    case re:run(Tail, "^[0-9A-Fa-f]", [{capture,none}]) of
        match -> <<"\\",NStr/binary," ">>;
        nomatch -> <<"\\",NStr/binary>>
    end.

join_map(Fun, List, Sep) ->
    uni:join(lists:map(Fun, List), Sep).

%% ------------------------------------------------------------------------------
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2014, Sebastian Egner.
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

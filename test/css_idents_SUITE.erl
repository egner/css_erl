%% -*- mode: erlang; coding: us-ascii; indent-tabs-mode: nil -*-
%% vim: set filetype=erlang fileencoding=utf-8 expandtab sw=4 sts=4:
%%
%% Unit tests for css_idents.
%% SE, created 12-Dec-2014, in Erlang/OTP 17 (common_test).
%%
%% Just for laughs:
%% css_file_SUITE:read_file([{data_dir,"/usr/local"}]).

-module(css_idents_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         groups/0,
         css_idents/1,
         merge_idents/1,
         print_idents/1,
         read_ident_literals_css/1,
         read_ident_literals_yaws/1,
         read_ident_literals_erl/1
        ]).

all() ->
    [{group,all}].

groups() ->
    [{all,[sequence],
      [{group,utilities},
       {group,read_ident_literals_x}]},
     {utilities,[parallel],
      [css_idents,
       merge_idents,
       print_idents]},
     {read_ident_literals_x,[parallel],
      [read_ident_literals_css,
       read_ident_literals_yaws,
       read_ident_literals_erl]}
    ].

%% -- tests --

css_idents(_Config) ->
    C = fun css_idents:css_idents/1,
    [] = C(""),
    [] = C(["",<<>>,[""]]),
    [<<"abc">>] = C(["",<<>>,["abc"," "]]),
    [<<"abc">>,<<"def">>] = C(["",<<>>,["abc"," ","def"]]),
    [<<224>>] = C([192,128]),
    [<<224>>] = C([224]), % Latin-1 fallback
    ok.

merge_idents(_Config) ->
    [{ident,<<"a">>,[{"f1",[1,2]}]}] =
        css_idents:merge_idents([{ident,<<"a">>,[{"f1",[1]}]},
                                 {ident,<<"a">>,[{"f1",[2]}]}]),
    ok.

print_idents(_Config) ->
    Idents = [{ident,<<"a">>,[]},
              {ident,<<"b">>,[{"bfile",[-1]}]},
              {ident,<<"c">>,[{"cfile1",[1]},{"cfile2",[2]}]}],
    Expected = <<"a\n\nb\n    bfile\n\nc\n    cfile1:1\n    cfile2:2\n\n">>,
    Expected = css_idents:print_idents(Idents),
    ok.

read_ident_literals_css(Config) ->
    Expected =
        [flipped, mydiv1, mydiv2, mydiv3, mydiv4, mydiv5, mydiv6,
         mydiv7, 'no-padding', num,overlapping, type, whatnot2,
         whatnot2, whatnot4, "hello-" ++ "world"],
    DataDir = ?config(data_dir, Config),
    File = filename:join(DataDir, "somedefs2.css"),
    {ok,Idents} = css_idents:read_ident_literals_css(File),
    ok = check_idents(Idents, Expected).

read_ident_literals_yaws(Config) ->
    case code:ensure_loaded(yaws_html) of
        {module,_} -> do_read_ident_literals_yaws(Config);
        {error,nofile} -> {skip,no_yaws_html}
    end.

do_read_ident_literals_yaws(Config) ->
    Expected = [unknown1, unknown2, whatnot1],
    DataDir = ?config(data_dir, Config),
    File = filename:join(DataDir, "somethings.html"),
    {ok,Idents} = css_idents:read_ident_literals_yaws(File),
    ok = check_idents(Idents, Expected).

read_ident_literals_erl(Config) ->
    Expected =
        [a, abc, all, b, bfile, c, cfile1, cfile2, css, css_idents,
         css_idents_suite, data_dir, def, erl, f1, flipped, group,
         'hello-', html, ident, merge_idents, mydiv1, mydiv2, mydiv3,
         mydiv4, mydiv5, mydiv6, mydiv7, 'no-padding', not1, num, ok,
         overlapping, parallel, print_idents, read_ident_literals_css,
         read_ident_literals_erl, read_ident_literals_x,
         read_ident_literals_yaws, sequence, skip, somedefs2, somethings,
         type, unknown1, unknown2, utilities, what, whatnot1, whatnot2,
         whatnot4, world, yaws_html, no_yaws_html],
    DataDir = ?config(data_dir, Config),
    File = filename:join(DataDir, "../css_idents_SUITE.erl"),
    {ok,Idents} = css_idents:read_ident_literals_erl(File, []),
    ok = check_idents(Idents, Expected).

check_idents(Idents, Expected) ->
    css_util:check_diff(
      [I || {ident,I,_} <- Idents],
      lists:usort([css_util:to_utf8(I) || I <- Expected])).



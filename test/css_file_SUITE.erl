%% -*- mode: erlang; coding: us-ascii; indent-tabs-mode: nil -*-
%% vim: set filetype=erlang fileencoding=utf-8 expandtab sw=4 sts=4:
%%
%% Unit tests for css_file.
%% SE, created 12-Dec-2014, in Erlang/OTP 17 (common_test).
%%
%% Just for laughs:
%% css_file_SUITE:read_file([{data_dir,"/usr/local"}]).

-module(css_file_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         groups/0,
         read_file/1,
         read_file_empty/1,
         write_file/1
        ]).

%% for interactive use
-export([write_term/2]).

all() ->
    [{group,all}].

groups() ->
    [{all,[sequence],[read_file,read_file_empty,write_file]}].

%% -- tests --

%% Read all .css files from data_dir. Check the resulting Erlang term
%% if there is a .css_erl file of the same name.
read_file(Config) ->
    DataDir = ?config(data_dir, Config),
    ok = check_files(Config, fun check_read_file/2, find_css_files(DataDir)).

check_read_file(_Config, CssInfile) ->
    case css_file:read_file(CssInfile) of
        {ok,Css} ->
            %% check against corresponding .css_erl file, if there is one
            ExpectedCssInfile = filename:rootname(CssInfile) ++ ".css_erl",
            case read_term(ExpectedCssInfile) of
                {ok,ExpectedCss} ->
                    io:fwrite(";; ~p.~n", [ExpectedCssInfile]),
                    css_util:check_diff(Css, ExpectedCss);
                false ->
                    ok
            end;
        Error={error,_,_} ->
            Error
    end.

%% Read an empty file.
read_file_empty(Config) ->
    %% Create a empty file, so we do not need to check it in.
    PrivDir = ?config(priv_dir, Config),
    EmptyIofile = filename:join(PrivDir, "empty.css"),
    {ok,_} = {file:write_file(EmptyIofile, <<>>),EmptyIofile},
    {ok,[]} = css_file:read_file(EmptyIofile),
    {ok,_} = {file:delete(EmptyIofile),EmptyIofile},
    ok.

%% Write a copy to priv_dir for each .css file in data_dir. The function
%% css_file:write_file/2 also reads the result back and compares it with
%% the Erlang term representing CSS.
write_file(Config) ->
    DataDir = ?config(data_dir, Config),
    ok = check_files(Config, fun check_write_file/2, find_css_files(DataDir)).

check_write_file(Config, CssInfile) ->
    PrivDir = ?config(priv_dir, Config),
    case css_file:read_file(CssInfile) of
        {ok,Css} ->
            Iofile = filename:join(PrivDir, filename:basename(CssInfile)),
            case css_file:write_file(Iofile, Css) of
                ok ->
                    case file:delete(Iofile) of
                        ok -> ok;
                        {error,Posix} -> {error,Posix,[{iofile,Iofile}]}
                    end;
                Error={error,_,_} ->
                    Error
            end;
        Error={error,_,_} ->
            Error
    end.

%% -- helper -

find_css_files(TopDir) -> % [file:filename_all()]
    lists:usort(
      filelib:fold_files(
        TopDir,
        "\\.[cC][sS][sS]$",
        true,
        fun (F, Acc) -> [F|Acc] end,
        [])).

check_files(Config, CheckFun, Files) -> % ok|{error,_,_}
    Errors =
        lists:foldl(
          fun (File, ErrorsAcc) ->
                  io:fwrite("; ~p~n", [File]),
                  case CheckFun(Config, File) of
                      ok -> ErrorsAcc;
                      Error={error,_,_} ->
                          io:fwrite("~n*** ERROR ***~n~p.~n~n", [Error]),
                          [File|ErrorsAcc]
                  end
          end,
          [],
          Files),
    io:fwrite("~n~n--------~n", []),
    case Errors of
        [] ->
            io:fwrite("OK: ~b files.~n", [length(Files)]),
            ok;
        _ ->
            io:fwrite("*** ERRORS: ~b of ~b files.~n",
                      [length(Errors), length(Files)]),
            {error,Errors}
    end.

write_term(Outfile, Term) ->
    Data = io_lib:fwrite("~p.~n", [Term]),
    {ok,_} = {file:write_file(Outfile, Data),Outfile}.

read_term(Infile) -> % {ok,Term}|false
    case {file:consult(Infile),Infile} of
        {{ok,[Term]},_} -> {ok,Term};
        {{error,enoent},_} -> false
    end.

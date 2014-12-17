%% -*- mode: erlang; coding: us-ascii; indent-tabs-mode: nil -*-
%% vim: set filetype=erlang fileencoding=utf-8 expandtab sw=4 sts=4:
%% Refer to license at the end of this file.
%%
%% Some utilities for the css_erl library.
%% SE, created 10-Dec-2014, Erlang/OTP 17.
%%
%% This module contains library functions equivalent to those used
%% in the original project, which has not been released Open Source
%% at this point in time.

-module(css_util).

-export([read_file/1,
         write_file_utf8/2,
         to_utf8/1,
         join/2,
         to_codepoints/1,
         strip2/1,
         check_diff/2]).

%% -- file --

%% Read a file, returning {ok,binary()}|{error,Reason,[Detail]}.
read_file(Infile) ->
    case file:read_file(Infile) of
        Ok={ok,_} -> Ok;
        {error,Reason} -> {error,Reason,[{infile,Infile}]}
    end.

%% Write UTF-8 into a file, returning ok|{error,Reason,[Detail]}.
write_file_utf8(Outfile, Utf8Iolist) ->
    case file:write_file(Outfile, to_utf8(Utf8Iolist)) of
        ok -> ok;
        {error,Reason} -> {error,Reason,[{outfile,Outfile}]}
    end.

%% -- UTF-8 vs. Unicode codepoints vs. Utf8Iolist --

%% Flatten into binary() representing UTF-8.
%% Binaries in the input are interpreted as UTF-8 if this is
%% possible, otherwise they are interpreted as Latin-1.
to_utf8(Utf8Iolist) ->
    to_utf8(Utf8Iolist, <<>>).

to_utf8([H|T], Acc) when is_integer(H), H < 128 ->
    to_utf8(T, <<Acc/binary,H>>);
to_utf8([H|T], Acc) when is_integer(H) ->
    to_utf8(T, <<Acc/binary,H/utf8>>);
to_utf8([H|T], Acc) when is_binary(H) ->
    case is_utf8(H) of
        true -> to_utf8(T, <<Acc/binary,H/binary>>);
        false -> to_utf8(T, to_utf8(binary_to_list(H), Acc))
    end;
to_utf8([H|T], Acc) when is_list(H) ->
    to_utf8(T, to_utf8(H, Acc));
to_utf8([H|T], Acc) when is_atom(H) ->
    to_utf8(T, to_utf8(atom_to_list(H), Acc)); % Latin-1
to_utf8([], Acc) ->
    Acc;
to_utf8(Any, Acc) when not is_list(Any) ->
    to_utf8([Any], Acc).

is_utf8(<<_/utf8,More/binary>>) -> is_utf8(More);
is_utf8(<<>>) -> true;
is_utf8(_) -> false.



%% Join the utf8_iolist()s with Sep as a separator.
join(Args, Sep) ->
    join(Args, Sep, []).

join([Arg|More], Sep, Acc) ->
    %% Unfortunately, we need to flatten Arg to decide if it is empty.
    case to_utf8(Arg) of
        <<>> -> join(More, Sep, Acc);
        Utf8 when Acc =:= [] -> join(More, Sep, [Utf8]);
        Utf8 -> join(More, Sep, [Utf8,Sep|Acc])
    end;
join([], _Sep, Acc) ->
    lists:reverse(Acc).

%% Flatten into list of Unicode codepoints.
to_codepoints(Utf8Iolist) ->
    to_codepoints(to_utf8(Utf8Iolist), []).

to_codepoints(<<Cp/utf8,More/binary>>, Acc) ->
    to_codepoints(More, [Cp|Acc]);
to_codepoints(<<>>, Acc) ->
    lists:reverse(Acc).

%% Strip "\t\n\f\r " from both ends.
strip2(String) ->
    lists:reverse(strip1(lists:reverse(strip1(String)))).

strip1([H|T]) when H =:= 9; H =:= 10; H =:= 12; H =:= 13; H =:= 32 -> strip1(T);
strip1(String) -> String.

%% -- utils --

%% Compares X and Y recursively and returns ok|{error,Reason,[Detail]}
%% where the details indicate the substructure that are not equal.
check_diff(X, Y) ->
    cd(X, Y, []).

cd([XH|XT], [YH|YT], Path) ->
    case cd(XH, YH, merge_path([{nth,1}|Path])) of
        ok -> cd(XT, YT, merge_path([{nthtail,1}|Path]));
        Error={error,_,_} -> Error
    end;
cd(X, Y, Path) when is_tuple(X), is_tuple(Y) ->
    case tuple_size(X) =:= tuple_size(Y) of
        true  -> cd_tuple(X, Y, 1, tuple_size(X), Path);
        false -> cd_error(tuple_size, X, Y, Path,
                          [{tuple_size_left,tuple_size(X)},
                           {tuple_size_right,tuple_size(Y)}])
    end;
cd(X, Y, Path) when is_integer(X), is_integer(Y) ->
    case X =:= Y of
        true  -> ok;
        false -> cd_error(integer_value, X, Y, Path, [])
    end;
cd(X, Y, Path) when is_number(X), is_number(Y) ->
    case X == Y of % maybe with tolerance
        true  -> ok;
        false -> cd_error(number_value, X, Y, Path, [])
    end;
cd(X, Y, Path) ->
    case X =:= Y of
        true  -> ok;
        false -> cd_error(term, X, Y, Path, [])
    end.

cd_error(Reason, X, Y, Path, More) ->
    {error,Reason,[{left,X},{right,Y},{path,lists:reverse(Path)}|More]}.

cd_tuple(X, Y, K, N, Path) when K =< N ->
    case cd(element(K, X), element(K, Y), [{element,K}|Path]) of
        ok -> cd_tuple(X, Y, K + 1, N, Path);
        Error={error,_,_} -> Error
    end;
cd_tuple(_, _, _, _, _) ->
    ok.

merge_path([{nth,K},{nthtail,N}|Path]) -> [{nth,N + K}|Path];
merge_path([{nthtail,K},{nthtail,N}|Path]) -> [{nthtail,N + K}|Path];
merge_path(Path) -> Path.

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

%% -*- mode: erlang; coding: us-ascii; indent-tabs-mode: nil -*-
%% vim: set filetype=erlang fileencoding=utf-8 expandtab sw=4 sts=4:
%% Refer to license at the end of this file.
%%
%% Scanner for CSS 2.1 (leex grammar).
%% SE, created 2-Sep-2013, Erlang/OTP R16B03 (leex).
%%
%% This syntax is converted from [1] G.2 to leex format, cf. [2]:
%% - Conversion required capitalizing the regexp names.
%% - Renaming created a name collision for for h, s, w, renamed Hx, Sx, Wx.
%% - '%' starts a comment in leex.
%% - Space (' ') in rhs of a definition must be replaced by '\s'.
%% - The regexp R{n,m} is not available in leex.
%% - "%option case-insensitive" does not exist in leex.
%% - Definitions in leex are inserted without enclosing ( ).
%% - All character classes had to be checked for quoting.
%% - We omit the no-back-up rules of [1], G.4.
%% - We implement a few extensions for CSS 3, marked by (1).
%% - We need to represent /* validator: PRAGMA */ comments.
%%
%% Extensions / Relaxations / Accepting broken CSS:
%% (1) Some CSS 3 extensions.
%% (2) '//' style comments.
%% (3) '~' as operator, like '+' but with elements in between
%% (4) Broken declarations like "padding: 8px; 7px 8px; 3px;".
%% (5) Extended pseudos like "audio:not([controls]) { }".
%% (6) Invalid '<style type "text/css">' and '</style>' tags.
%% (7) The "::before" selector. CSS3 uses '::' for pseudos.
%% (8) CSS 3 media queries.
%% (9) Other matching operators ~=, ^=, $=, *=
%% (10) Some @-defs need a ruleset others a declaration-list.
%% (11) Composed function names in declarations.
%% (12) Selector inside expr.
%% (13) ' ' as an operator in expr.
%% (14) Invalid '*..' as property name (IE9).
%% (15) Invalid '#.x' as selector.
%% (16) @-definitions.
%% (17) Broken "body {background= #a0f0a0}".
%% (18) "<?lua .. ?>" extension.
%% (19) Broken "x, {a: b;}"
%%
%% References:
%% [1] http://www.w3.org/TR/CSS21/grammar.html
%% [2] http://erlang.org/doc/man/leex.html
%% [3] http://www.w3.org/TR/css3-syntax

%% END OF HEADER TO COPY.

%% ================================================================================

Definitions.

%% option case-insensitive

Hx              = ([0-9a-fA-F])
Hx2             = ({Hx}{Hx})
Hx3             = ({Hx}{Hx}{Hx})
Hx1to6          = ({Hx}|{Hx2}|{Hx3}|{Hx2}{Hx2}|{Hx2}{Hx3}|{Hx3}{Hx3})
Nonascii        = ([\240-\377])
Unicode         = (\\({Hx1to6})(\r\n|[\s\t\r\n\f])?)
Escape          = ({Unicode}|\\[^\r\n\f0-9a-fA-F])
Nmstart         = ([_a-zA-Z]|{Nonascii}|{Escape})
Nmchar          = ([_a-zA-Z0-9-]|{Nonascii}|{Escape})
String1         = (\"([^\n\r\f\\\"]|\\{Nl}|{Escape})*\")
String2         = (\'([^\n\r\f\\\']|\\{Nl}|{Escape})*\')
Badstring1      = (\"([^\n\r\f\\\"]|\\{Nl}|{Escape})*\\?)
Badstring2      = (\'([^\n\r\f\\\']|\\{Nl}|{Escape})*\\?)
Badcomment1     = (\/\*[^*]*\*+([^/*][^*]*\*+)*)
Badcomment2     = (\/\*[^*]*(\*+[^/*][^*]*)*)
Badcomment3     = (\/\/[^\n\r\f]*{Nl})
Baduri1         = (url\({Wx}([!#$%&*-\[\]-~]|{Nonascii}|{Escape})*{Wx})
Baduri2         = (url\({Wx}{String}{Wx})
Baduri3         = (url\({Wx}{Badstring})
Comment         = (\/\*[^*]*\*+([^/*][^*]*\*+)*\/)
ValidatorPragma = (\/\*[\s\t]*validator:[\s\t]*([0-9A-Za-z_,+-]*)[\s\t]*\*\/)
Ident           = (-?{Nmstart}{Nmchar}*)
Name            = ({Nmchar}+)
Num             = ([0-9]+|[0-9]*\.[0-9]+)
String          = ({String1}|{String2})
Badstring       = ({Badstring1}|{Badstring2})
Badcomment      = ({Badcomment1}|{Badcomment2}|{Badcomment3})
Baduri          = ({Baduri1}|{Baduri2}|{Baduri3})
Url             = (([!#$\%&*-~]|{Nonascii}|{Escape})*)
Sx              = ([\s\t\r\n\f]+)
Wx              = ({Sx}?)
Nl              = (\n|\r\n|\r|\f)

Zero0to4        = ((0|00|000|0000)?)
A               = (a|A|\\{Zero0to4}(41|61)(\r\n|[\s\t\r\n\f])?)
C               = (c|C|\\{Zero0to4}(43|63)(\r\n|[\s\t\r\n\f])?)
D               = (d|D|\\{Zero0to4}(44|64)(\r\n|[\s\t\r\n\f])?)
E               = (e|E|\\{Zero0to4}(45|65)(\r\n|[\s\t\r\n\f])?)
G               = (g|G|\\{Zero0to4}(47|67)(\r\n|[\s\t\r\n\f])?|\\(g|G))
H               = (h|H|\\{Zero0to4}(48|68)(\r\n|[\s\t\r\n\f])?|\\(h|H))
I               = (i|I|\\{Zero0to4}(49|69)(\r\n|[\s\t\r\n\f])?|\\(i|I))
K               = (k|K|\\{Zero0to4}(4b|6b)(\r\n|[\s\t\r\n\f])?|\\(k|K))
L               = (l|L|\\{Zero0to4}(4c|6c)(\r\n|[\s\t\r\n\f])?|\\(l|L))
M               = (m|M|\\{Zero0to4}(4d|6d)(\r\n|[\s\t\r\n\f])?|\\(m|M))
N               = (n|N|\\{Zero0to4}(4e|6e)(\r\n|[\s\t\r\n\f])?|\\(n|N))
O               = (o|O|\\{Zero0to4}(4f|6f)(\r\n|[\s\t\r\n\f])?|\\(o|O))
P               = (p|P|\\{Zero0to4}(50|70)(\r\n|[\s\t\r\n\f])?|\\(p|P))
R               = (r|R|\\{Zero0to4}(52|72)(\r\n|[\s\t\r\n\f])?|\\(r|R))
S               = (s|S|\\{Zero0to4}(53|73)(\r\n|[\s\t\r\n\f])?|\\(s|S))
T               = (t|T|\\{Zero0to4}(54|74)(\r\n|[\s\t\r\n\f])?|\\(t|T))
U               = (u|U|\\{Zero0to4}(55|75)(\r\n|[\s\t\r\n\f])?|\\(u|U))
X               = (x|X|\\{Zero0to4}(58|78)(\r\n|[\s\t\r\n\f])?|\\(x|X))
Y               = (y|Y|\\{Zero0to4}(59|79)(\r\n|[\s\t\r\n\f])?|\\(y|Y))
Z               = (z|Z|\\{Zero0to4}(5a|7a)(\r\n|[\s\t\r\n\f])?|\\(z|Z))

CharToken       = ([\(\)\*\+\,\-\.\/\:\;\=\>\[\]\{\}\~\#])
BadChar         = ([^\)\*\+\,\-\.\/\:\;\=\>\[\]\{\}])

%% pacifying Dialyzer on leexinc.hrl
Impossible1     = (===IMPOSSIBLE1===)
Impossible2     = (===IMPOSSIBLE2===)
Impossible3     = (===IMPOSSIBLE3===)
Impossible4     = (===IMPOSSIBLE4===)
Impossible5     = (===IMPOSSIBLE5===)

%% ================================================================================

Rules.

{Sx} : {token,{'S',TokenLine}}. % non-empty whitespace

{ValidatorPragma}              : {token,{'VALIDATOR',TokenLine,to_validator(TokenChars)}}.
\/\*[^*]*\*+([^/*][^*]*\*+)*\/ : skip_token.

\/\/[^\n\r\f]*{Nl} : skip_token. % (2)
{Badcomment}       : skip_token.

<style\stype\s\"text/css\"> : skip_token. % (6)
<\/style>                   : skip_token. % (6)

<!--                      : {token,{'CDO',TokenLine}}.
-->                       : {token,{'CDC',TokenLine}}.
~=                        : {token,{'INCLUDES',TokenLine}}.
\|=                       : {token,{'DASHMATCH',TokenLine}}.
\^=                       : {token,{'CARETMATCH',TokenLine}}.  % (9)
\~=                       : {token,{'TILDEMATCH',TokenLine}}.  % (9)
\$=                       : {token,{'DOLLARMATCH',TokenLine}}. % (9)
\*=                       : {token,{'STARMATCH',TokenLine}}.   % (9)

{String}                  : {token,{'STRING',TokenLine,string_to_unicode(TokenChars)}}.
{Badstring}               : {token,{'BAD_STRING',TokenLine,TokenChars}}.

{O}{N}{L}{Y}              : {token,{'ONLY',TokenLine}}. % (8)
{N}{O}{T}                 : {token,{'NOT',TokenLine}}.  % (8)
{A}{N}{D}                 : {token,{'AND',TokenLine}}.  % (8)

{Ident}                   : {token,{'IDENT',TokenLine,ident_to_unicode(TokenChars)}}.

#{Name}                   : {token,{'HASH',TokenLine,hash_to_unicode(TokenChars)}}.

@{I}{M}{P}{O}{R}{T}       : {token,{'IMPORT_SYM',TokenLine}}.
@{P}{A}{G}{E}             : {token,{'PAGE_SYM',TokenLine}}.
@{M}{E}{D}{I}{A}          : {token,{'MEDIA_SYM',TokenLine}}.
@charset\s                : {token,{'CHARSET_SYM',TokenLine}}.
@font-face                : {token,{'AT_SYM2',TokenLine,ident_to_unicode(TokenChars)}}. % (10)
@-ms-viewport             : {token,{'AT_SYM2',TokenLine,ident_to_unicode(TokenChars)}}. % (10)
@{Ident}                  : {token,{'AT_SYM',TokenLine,ident_to_unicode(TokenChars)}}.

!({Wx}|{Comment})*{I}{M}{P}{O}{R}{T}{A}{N}{T} : {token,{'IMPORTANT_SYM',TokenLine}}.

{Num}{E}{M}               : {token,{'EMS',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{E}{X}               : {token,{'EXS',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{P}{X}               : {token,{'LENGTH',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{C}{M}               : {token,{'LENGTH',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{M}{M}               : {token,{'LENGTH',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{I}{N}               : {token,{'LENGTH',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{P}{T}               : {token,{'LENGTH',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{P}{C}               : {token,{'LENGTH',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{D}{E}{G}            : {token,{'ANGLE',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{R}{A}{D}            : {token,{'ANGLE',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{G}{R}{A}{D}         : {token,{'ANGLE',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{M}{S}               : {token,{'TIME',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{S}                  : {token,{'TIME',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{H}{Z}               : {token,{'FREQ',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{K}{H}{Z}            : {token,{'FREQ',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}{D}{P}{I}            : {token,{'RESOLUTION',TokenLine,to_num(TokenLine, TokenChars)}}. % (8)
{Num}{D}{P}{C}{M}         : {token,{'RESOLUTION',TokenLine,to_num(TokenLine, TokenChars)}}. % (8)
{Num}{Ident}              : {token,{'DIMENSION',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}\%                   : {token,{'PERCENTAGE',TokenLine,to_num(TokenLine, TokenChars)}}.
{Num}                     : {token,{'NUMBER',TokenLine,to_num(TokenLine, TokenChars)}}.

url\({Wx}{String}{Wx}\)   : {token,{'URI',TokenLine,{string,url_to_unicode1(TokenChars)}}}.
url\({Wx}{Url}{Wx}\)      : {token,{'URI',TokenLine,{url,url_to_unicode2(TokenChars)}}}.
{Baduri}                  : {token,{'BAD_URI',TokenLine,TokenChars}}.

{Ident}\(                 : {token,{'FUNCTION',TokenLine,ident_to_unicode(TokenChars)}}.

<\?lua\s[^\?]*\?>         : {token,{'LUA',TokenLine,lua_to_unicode(TokenChars)}}. % (18)

\:\:                      : {token,{'PSEUDO',TokenLine}}. % (7)
{CharToken}               : {token,{list_to_atom(TokenChars),TokenLine}}.

{BadChar}                 : {token,{'BAD_CHAR',TokenLine,TokenChars}}.

%% pacifying Dialyzer on leexinc.hrl
{Impossible1} : {token,{'IMPOSSIBLE1',TokenLine,TokenChars},[]}.
{Impossible2} : {skip_token,[]}.
{Impossible3} : {end_token,{'IMPOSSIBLE3',TokenLine,TokenChars}}.
{Impossible4} : {end_token,{'IMPOSSIBLE4',TokenLine,TokenChars},[]}.
{Impossible5} : {error,"IMPOSSIBLE5"}.

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

%% ================================================================================

Erlang code.

-compile(no_native). % HiPE can't handle the size of the generated scanner.

-export([value/1,
         ident_to_atom/1,
         ident_to_at_sym/1,
         hash_to_hexcolor/1,
         function_name/1,
         selector/1,
         flatten_expr/1,
         apply_unary_operator/2,
         remove_comments/1,
         fix_attr/1]).

%% We work with strings as lists of Unicode codepoints.

to_num(TokenLine, TokenChars) ->
    NumRe = "^([0-9]*\\.[0-9]+|[0-9]+)(.*)$",
    Opts = [{capture,all_but_first,list}],
    {{match,[NumStr,Unit]},_} = {re:run(TokenChars, NumRe, Opts),TokenChars},
    Num = case string:chr(NumStr, $.) of
              0 -> list_to_integer(NumStr);
              1 -> list_to_float("0" ++ NumStr);
              _ -> list_to_float(NumStr)
          end,
    case Unit of
        "" -> Num;
        _ -> {Num,codepoints_to_atom(TokenLine, Unit)}
    end.

codepoints_to_atom(Line, Codepoints) ->
    case length(Codepoints) =< 255 andalso
        lists:all(fun (C) -> 0 =< C andalso C =< 255 end, Codepoints)
    of
        true -> list_to_atom(string:to_lower(Codepoints));
        false -> exit({bad_atom,[{input,Codepoints},{line,Line}]})
    end.

url_to_unicode1(TokenChars) ->
    "url(" ++ ContentRparen = TokenChars,
    ")" ++ RevContent = lists:reverse(ContentRparen),
    string_to_unicode(lists:reverse(RevContent)).

url_to_unicode2(TokenChars) ->
    "url(" ++ ContentRparen = TokenChars,
    ")" ++ RevContent = lists:reverse(ContentRparen),
    lists:reverse(RevContent).

string_to_unicode(TokenChars) ->
    [Quote|ContentQuote] = to_unicode(TokenChars),
    [Quote|RevContent] = lists:reverse(ContentQuote),
    lists:reverse(RevContent).

lua_to_unicode(TokenChars) ->
    "<?lua"++ Content1 = to_unicode(TokenChars),
    ">?" ++ RevContent2 = lists:reverse(Content1),
    css_util:strip2(lists:reverse(RevContent2)).

ident_to_unicode(TokenChars) ->
    string:to_lower(to_unicode(TokenChars)).

hash_to_unicode("#" ++ TokenChars) ->
    ident_to_unicode(TokenChars).

to_validator(TokenChars) ->
    ValidatorRe = "/\\*[ \t]*validator:[ \t]*([0-9A-Za-z_,+-]*)[ \t]*\\*/",
    case re:run(TokenChars, ValidatorRe, [{capture,all_but_first,list}]) of
        {match,[Pragma]} -> {'/*validator:*/',Pragma};
        nomatch -> exit({bad_validator_pragma,[{input,TokenChars}]})
    end.

to_unicode(TokenChars) ->
    unescape(TokenChars, []).

%% continued string
unescape("\\\r\n" ++ T, Acc) -> unescape(T, Acc);
unescape("\\\n" ++ T, Acc) -> unescape(T, Acc);
unescape("\\\r" ++ T, Acc) -> unescape(T, Acc);
unescape("\\\f" ++ T, Acc) -> unescape(T, Acc);
%% escape with specific meaning
unescape("\\t" ++ T, Acc) -> unescape(T, ["\t"|Acc]);
unescape("\\n" ++ T, Acc) -> unescape(T, ["\n"|Acc]);
unescape("\\f" ++ T, Acc) -> unescape(T, ["\f"|Acc]);
unescape("\\r" ++ T, Acc) -> unescape(T, ["\r"|Acc]);
unescape("\\" ++ Escape, Acc) ->
    UnicodeRe = "^[0-9a-fA-F]{1,6}(\r\n|[\s\t\r\n\f])?",
    case re:run(Escape, UnicodeRe, [{capture,first,list}]) of
        {match,[Hex]} ->
            unescape(lists:nthtail(length(Hex), Escape),
                     [[list_to_integer(css_util:strip2(Hex), 16)]|Acc]);
        nomatch -> % skip the backslash
            unescape(Escape, Acc)
    end;
%% any other codepoint
unescape([H|T], Acc) -> unescape(T, [[H]|Acc]);
unescape([], Acc) -> lists:append(lists:reverse(Acc)).

%% -- utilities for css_yecc.erl --

value({_TokenName,_Line,Value}) ->
    Value.

ident_to_atom({'IDENT',Line,Codepoints}) ->
    codepoints_to_atom(Line, Codepoints).

ident_to_at_sym({_,Line,"@"++Codepoints}) ->
    ident_to_atom({'IDENT',Line,Codepoints}).

hash_to_hexcolor({'HASH',Line,Hex}) ->
    HexcolorRe = "^([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$",
    case re:run(Hex, HexcolorRe, [{capture,none}]) of
        match   -> {hexcolor,string:to_upper(Hex)};
        nomatch -> exit({syntax_error_in_hexcolor,
                         [{input,Hex},
                          {line,Line},
                          {regexp,HexcolorRe}]})
    end.

function_name({'FUNCTION',_Line,Codepoints}) ->
    [$(|RevName] = lists:reverse(Codepoints),
    lists:reverse(RevName).

selector(Items) ->
    selector(Items, []).

selector([Combinator,SimpleSelector|Items], Acc) ->
    selector(Items, [{Combinator,SimpleSelector}|Acc]);
selector([], Acc) ->
    lists:reverse(Acc).

%% Collects structures of {Op,X1,{Op,X2,..}} into {Op,[X1,X2,..]}
%% for all operators Op, at least ',', ' ' and '/'.
flatten_expr({Op,X1,X2})
  when Op =:= ','; Op =:= ' '; Op =:= '/' ->
    Y1 = flatten_expr(X1),
    case flatten_expr(X2) of
        {Op2,Y2s} when Op2 =:= Op -> {Op,[Y1|Y2s]};
        Y2 -> {Op,[Y1,Y2]}
    end;
flatten_expr(X) ->
    X.

apply_unary_operator('+', Value) ->
    Value;
apply_unary_operator('-', Number)
  when is_number(Number) ->
    -Number;
apply_unary_operator('-', {Number,Unit})
  when is_number(Number), is_atom(Unit) ->
    {-Number,Unit}.

remove_comments(Items) ->
    remove_comments(Items, [], 0).

remove_comments(['<!--'|Items], Acc, N) ->
    remove_comments(Items, Acc, N + 1);
remove_comments(['-->'|Items], Acc, N) ->
    remove_comments(Items, Acc, max(0, N - 1));
remove_comments([Item|Items], Acc, 0) ->
    remove_comments(Items, [Item|Acc], 0);
remove_comments([_Item|Items], Acc, N) when N > 0 ->
    remove_comments(Items, Acc, N);
remove_comments([], Acc, _) ->
    lists:reverse(Acc).

fix_attr({attr,Ident,Rel,{ident,String}})
  when Rel =:= '^='; Rel =:= '$='; Rel =:= '*=' ->
    {attr,Ident,Rel,{string,String}};
fix_attr(Attr={attr,_Ident,_Rel,_Rhs}) ->
    Attr.

%% -*- mode: erlang; coding: us-ascii; indent-tabs-mode: nil -*-
%% vim: set filetype=erlang fileencoding=utf-8 expandtab sw=4 sts=4:
%% Refer to license at the end of this file.
%%
%% Parser for CSS 2.1 (yecc grammar).
%% SE, created 3-Sep-2013, Erlang/OTP R16B03 (yecc).
%%
%% This syntax is converted from [1] G.1 to yecc format, cf. [2]:
%% - Yecc does not understand EBNF. Unrolled manually.
%% - Yecc does not allow empty productions. Unrolled manually.
%%   The only remaining case is 'stylesheet' which may be empty,
%%   which cannot be expressed in the grammar of yecc directly.
%% - In [1] G.1 the non-terminal media_list uses the terminal COMMA.
%%   This is simply an error in [1], we corrected it to ','.
%% - There are several shift/reduce conflicts, resolved for shift.
%% - We implement a few extension, for CSS 3 and things that are
%%   'out there', marked by (1), (2) etc. Cf. css_leex.xrl.
%% - We structure 'expr' by precedence as ',' > ' ' > '/'.
%%
%% References:
%% [1] http://www.w3.org/TR/CSS21/grammar.html
%% [2] http://erlang.org/doc/man/yecc.html
%% [3] http://www.w3.org/TR/css3-syntax
%% [4] http://www.w3.org/TR/2012/REC-css3-mediaqueries-20120619
%% [5] http://www.w3schools.com/cssref/css_selectors.asp
%% [6] http://www.w3.org/TR/selectors/#attribute-substrings

%% END OF HEADER TO COPY.

Nonterminals
   at at_sym_s_star attrib attrib1 attrib1_s_star attrib2 attrib2_s_star
   cbrace_s_star cd cd_s_star cd_s_star_plus charset_def class combinator
   comma_s_star declaration declaration_end declaration_entries declaration_list
   element_name expr expr1 expr2 expr3 function function_s_star hexcolor ident
   ident_s_star import import_list import_list_plus import_sym_s_star imports
   media media_sym_s_star obrace_s_star obrack_s_star page
   media_query_list1 media_query media_type_s_star expression_list and_s_star
     only_or_not_s_star only_or_not expression colon_s_star oparen_s_star
     cparen_s_star media_feature_s_star
   page_sym_s_star percentage_s_star prio property pseudo pseudo_page rmp_list
   rmp_list_plus rmps ruleset ruleset_list ruleset_or_media_or_page ruleset_plus
   s_or_cd s_or_cd_plus s_or_cds s_plus selector selector1 selector_list
   semicolon_s_star simple_selector simple_selector1 simple_selector1_plus
   slash_s_star string string_or_uri string_or_uri_s_star stylesheet stylesheet1
   term term1 term2 unary_operator uri validator validator_s_star
   pseudo_sym
   function2 function2_s_star equals_s_star at_sym2_s_star kv_list.

Terminals
   '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '=' '>' '[' ']' '{' '}' '~' '#'
   ANGLE AT_SYM AT_SYM2 CDC CDO CHARSET_SYM DASHMATCH DIMENSION EMS EXS FREQ FUNCTION
   HASH IDENT IMPORTANT_SYM IMPORT_SYM INCLUDES LENGTH MEDIA_SYM NUMBER PAGE_SYM
   PERCENTAGE S STRING TIME URI VALIDATOR % unused: BAD_STRING BAD_URI BAD_CHAR.
   ONLY NOT AND RESOLUTION % (8)
   PSEUDO % (7)
   CARETMATCH TILDEMATCH DOLLARMATCH STARMATCH % (9)
   LUA. % (18)

Rootsymbol
   stylesheet.


%% --- rules ---

stylesheet -> stylesheet1 : css_leex:remove_comments('$1').

stylesheet1 -> charset_def                        : '$1'.
stylesheet1 ->             s_or_cds               : '$1'.
stylesheet1 ->                      imports       : '$1'.
stylesheet1 ->                              rmps  : '$1'.
stylesheet1 -> charset_def s_or_cds               : '$1' ++ '$2'.
stylesheet1 -> charset_def          imports       : '$1' ++ '$2'.
stylesheet1 -> charset_def                  rmps  : '$1' ++ '$2'.
stylesheet1 ->             s_or_cds imports       : '$1' ++ '$2'.
stylesheet1 ->             s_or_cds         rmps  : '$1' ++ '$2'.
stylesheet1 ->                      imports rmps  : '$1' ++ '$2'.
stylesheet1 -> charset_def s_or_cds imports       : '$1' ++ '$2' ++ '$3'.
stylesheet1 -> charset_def s_or_cds         rmps  : '$1' ++ '$2' ++ '$3'.
stylesheet1 -> charset_def          imports rmps  : '$1' ++ '$2' ++ '$3'.
stylesheet1 ->             s_or_cds imports rmps  : '$1' ++ '$2' ++ '$3'.
stylesheet1 -> charset_def s_or_cds imports rmps  : '$1' ++ '$2' ++ '$3' ++ '$4'.

charset_def -> CHARSET_SYM STRING ';' : [{'@charset',css_leex:value('$2')}].

s_or_cds -> s_or_cd_plus : lists:append('$1').

s_or_cd_plus -> s_or_cd              : ['$1'].
s_or_cd_plus -> s_or_cd s_or_cd_plus : ['$1'|'$2'].

s_or_cd -> s_plus : [].
s_or_cd -> cd     : ['$1'].

imports -> import_list_plus : lists:append('$1').

import_list_plus -> import_list                  : ['$1'].
import_list_plus -> import_list import_list_plus : ['$1'|'$2'].

import_list -> import                : ['$1'].
import_list -> import cd_s_star_plus : ['$1'|'$2'].

rmps -> rmp_list_plus : lists:append('$1').

rmp_list_plus -> rmp_list               : ['$1'].
rmp_list_plus -> rmp_list rmp_list_plus : ['$1'|'$2'].

rmp_list -> ruleset_or_media_or_page                : ['$1'].
rmp_list -> ruleset_or_media_or_page cd_s_star_plus : ['$1'|'$2'].

cd_s_star_plus -> cd_s_star                : ['$1'].
cd_s_star_plus -> cd_s_star cd_s_star_plus : ['$1'|'$2'].

cd_s_star -> cd        : '$1'.
cd_s_star -> cd s_plus : '$1'.

cd -> CDO : '<!--'.
cd -> CDC : '-->'.

ruleset_or_media_or_page -> ruleset : '$1'.
ruleset_or_media_or_page -> media   : '$1'.
ruleset_or_media_or_page -> page    : '$1'.
ruleset_or_media_or_page -> at      : '$1'.

import -> import_sym_s_star string_or_uri_s_star                   semicolon_s_star : {'@import','$2',[]}.
import -> import_sym_s_star string_or_uri_s_star media_query_list1 semicolon_s_star : {'@import','$2','$3'}.

import_sym_s_star -> IMPORT_SYM.
import_sym_s_star -> IMPORT_SYM s_plus.

string_or_uri_s_star -> string_or_uri        : '$1'.
string_or_uri_s_star -> string_or_uri s_plus : '$1'.

string_or_uri -> string : '$1'.
string_or_uri -> uri    : '$1'.

media -> media_sym_s_star                   ruleset_list : {'@media',[],'$2'}.
media -> media_sym_s_star media_query_list1 ruleset_list : {'@media','$2','$3'}.

media_sym_s_star -> MEDIA_SYM.
media_sym_s_star -> MEDIA_SYM s_plus.

ruleset_list -> obrace_s_star              cbrace_s_star : [].
ruleset_list -> obrace_s_star ruleset_plus cbrace_s_star : '$2'.

ruleset_plus -> ruleset              : ['$1'].
ruleset_plus -> ruleset ruleset_plus : ['$1'|'$2'].

%%% CSS2:
%media_list -> medium                         : ['$1'].
%media_list -> medium comma_s_star media_list : ['$1'|'$3'].
%
%medium -> ident_s_star : '$1'.

%% -- begin CSS3 --

%% According to [4] media_query_list replaces media_list. However, the
%% former can also be entirely empty, unlike the latter.

%% media_query_list
%%  : S* [media_query [ ',' S* media_query ]* ]?
%%  ;
%% media_query
%%  : [ONLY | NOT]? S* media_type S* [ AND S* expression ]*
%%  | expression [ AND S* expression ]*
%%  ;
%% media_type
%%  : IDENT
%%  ;
%% expression
%%  : '(' S* media_feature S* [ ':' S* expr ]? ')' S*
%%  ;
%% media_feature
%%  : IDENT
%%  ;

media_query_list1 -> media_query                                     : ['$1'].
media_query_list1 -> media_query ','        media_query_list1        : ['$1'|'$3'].
media_query_list1 -> media_query ','        media_query_list1 s_plus : ['$1'|'$3'].
media_query_list1 -> media_query ',' s_plus media_query_list1        : ['$1'|'$4'].
media_query_list1 -> media_query ',' s_plus media_query_list1 s_plus : ['$1'|'$4'].

media_query ->                    media_type_s_star                 : {media_query,'$1',[]}.
media_query ->                    media_type_s_star expression_list : {media_query,'$1','$2'}.
media_query -> only_or_not_s_star media_type_s_star                 : {media_query,{'$1','$2'},[]}.
media_query -> only_or_not_s_star media_type_s_star expression_list : {media_query,{'$1','$2'},'$3'}.
media_query -> expression                                           : {media_query,any,['$1']}.
media_query -> expression expression_list                           : {media_query,any,['$1'|'$2']}.

media_type_s_star -> ident_s_star : '$1'.

expression_list -> and_s_star expression                 : ['$2'].
expression_list -> and_s_star expression expression_list : ['$2'|'$3'].

and_s_star -> AND        : 'and'.
and_s_star -> AND s_plus : 'and'.

only_or_not_s_star -> only_or_not        : '$1'.
only_or_not_s_star -> only_or_not s_plus : '$1'.

only_or_not -> ONLY : 'only'.
only_or_not -> NOT  : 'not'.

expression -> oparen_s_star media_feature_s_star colon_s_star expr cparen_s_star : {':','$2','$4'}.
expression -> oparen_s_star media_feature_s_star                   cparen_s_star : '$2'.

colon_s_star -> ':'        : ':'.
colon_s_star -> ':' s_plus : ':'.

oparen_s_star -> '('        : '('.
oparen_s_star -> '(' s_plus : '('.

cparen_s_star -> ')'        : ')'.
cparen_s_star -> ')' s_plus : ')'.

media_feature_s_star -> ident_s_star : '$1'.

%% -- end CSS3 --

page -> page_sym_s_star             declaration_list : {'@page',[],'$2'}.
page -> page_sym_s_star pseudo_page declaration_list : {'@page','$2','$3'}.

page_sym_s_star -> PAGE_SYM.
page_sym_s_star -> PAGE_SYM s_plus.

pseudo_page -> pseudo_sym ident_s_star : ['$2']. % (7)

% (1), (10)
at -> at_sym_s_star                            ruleset          : {'@R','$1','$2'}.
at -> at_sym_s_star  function                  ruleset_list     : {'@R','$1','$2','$3'}.
at -> at_sym_s_star  function validator_s_star ruleset_list     : {'@R','$1','$2','$3','$4'}.
at -> at_sym2_s_star                           declaration_list : {'@D','$1','$2'}.
at -> at_sym2_s_star function                  declaration_list : {'@D','$1','$2','$3'}.
at -> at_sym2_s_star function validator_s_star declaration_list : {'@D','$1','$2','$3','$4'}.
at -> at_sym_s_star ':' s_plus               expr               semicolon_s_star : {'@:','$1','$4'}. % (16)
at -> at_sym_s_star ':' s_plus oparen_s_star expr cparen_s_star semicolon_s_star : {'@:','$1','$5'}. % (16)

at_sym_s_star -> AT_SYM        : css_leex:ident_to_at_sym('$1').
at_sym_s_star -> AT_SYM s_plus : css_leex:ident_to_at_sym('$1').

at_sym2_s_star -> AT_SYM2        : css_leex:ident_to_at_sym('$1').
at_sym2_s_star -> AT_SYM2 s_plus : css_leex:ident_to_at_sym('$1').

slash_s_star -> '/'          : '/'.
slash_s_star -> '/' s_plus   : '/'.

comma_s_star -> ','        : ','.
comma_s_star -> ',' s_plus : ','.

combinator -> '+'        : '+'.
combinator -> '+' s_plus : '+'.
combinator -> '>'        : '>'.
combinator -> '>' s_plus : '>'.
combinator -> '~'        : '~'. % cf. css_leex.xrl, (3)
combinator -> '~' s_plus : '~'.

unary_operator -> '-' : '-'.
unary_operator -> '+' : '+'.

property -> ident_s_star     : '$1'.
property -> '*' ident_s_star : {'*','$2'}. % (14)

ruleset -> selector_list              declaration_list : {ruleset,'$1','$2'}.
% ruleset -> selector_list comma_s_star declaration_list : {ruleset,'$1','$3'}. % (19)

declaration_list -> obrace_s_star                     cbrace_s_star : [].
declaration_list -> obrace_s_star declaration_entries cbrace_s_star : '$2'.

declaration_entries -> declaration_end                     : '$1'.
declaration_entries -> declaration                         : case '$1' of bad4 -> []; _ -> ['$1'] end.
declaration_entries -> declaration_end declaration_entries : '$1' ++ '$2'.
declaration_entries -> declaration     declaration_entries : case '$1' of bad4 -> '$2'; _ -> ['$1'|'$2'] end.

declaration_end -> ';'                         : [].
declaration_end -> ';' s_plus                  : [].
declaration_end -> ';'        validator_s_star : ['$2'].
declaration_end -> ';' s_plus validator_s_star : ['$3'].

semicolon_s_star -> ';'        : ';'.
semicolon_s_star -> ';' s_plus : ';'.

obrace_s_star -> '{'.
obrace_s_star -> '{' s_plus.

cbrace_s_star -> '}'.
cbrace_s_star -> '}' s_plus.

selector_list -> selector                                             : [{selector,'$1'}].
selector_list -> selector comma_s_star                                : [{selector,'$1'}]. % (19)
selector_list -> selector comma_s_star selector_list                  : [{selector,'$1'}|'$3'].
selector_list -> selector                            validator_s_star : [{selector,'$1'},'$2'].
selector_list -> selector comma_s_star selector_list validator_s_star : [{selector,'$1'}|'$3'] ++ ['$4'].

selector -> selector1 : css_leex:selector([' '|'$1']).

selector1 -> simple_selector                             : ['$1'].
selector1 -> simple_selector        combinator selector1 : ['$1','$2'|'$3'].
selector1 -> simple_selector s_plus                      : ['$1'].
selector1 -> simple_selector s_plus            selector1 : ['$1',' '|'$3'].
selector1 -> simple_selector s_plus combinator selector1 : ['$1','$3'|'$4'].

simple_selector -> element_name                       : {'$1',[]}.
simple_selector -> element_name simple_selector1_plus : {'$1','$2'}.
simple_selector ->              simple_selector1_plus : {'*','$1'}.

simple_selector1_plus -> simple_selector1                       : ['$1'].
simple_selector1_plus -> simple_selector1 simple_selector1_plus : ['$1'|'$2'].

simple_selector1 -> HASH   : {'HASH',_Line,Ident} = '$1', {id,Ident}.
simple_selector1 -> class  : '$1'.
simple_selector1 -> attrib : '$1'.
simple_selector1 -> pseudo : '$1'.
simple_selector1 -> '#' class : {'#','$2'}. % (15)

class -> '.' ident : {class,'$2'}.

element_name -> IDENT : css_leex:ident_to_atom('$1').
element_name -> '*'   : '*'.

attrib -> obrack_s_star ident_s_star                               ']' : {attr,'$2'}.
attrib -> obrack_s_star ident_s_star attrib1_s_star attrib2_s_star ']' : css_leex:fix_attr({attr,'$2','$3','$4'}).

attrib1_s_star -> attrib1        : '$1'.
attrib1_s_star -> attrib1 s_plus : '$1'.

attrib1 -> '='       : '='.
attrib1 -> INCLUDES  : '~='.
attrib1 -> DASHMATCH : '|='.
attrib1 -> CARETMATCH  : '^='.
attrib1 -> TILDEMATCH  : '~='.
attrib1 -> DOLLARMATCH : '$='.
attrib1 -> STARMATCH   : '*='.

attrib2_s_star -> attrib2        : '$1'.
attrib2_s_star -> attrib2 s_plus : '$1'.

attrib2 -> ident  : {ident,'$1'}.
attrib2 -> string : '$1'.

obrack_s_star -> '['.
obrack_s_star -> '[' s_plus.

pseudo -> pseudo_sym ident                            : {'$1',{ident,'$2'}}.
pseudo -> pseudo_sym function_s_star      ')'         : {'$1',{function,'$2',[]}}.
pseudo -> pseudo_sym function_s_star expr ')'         : {'$1',{function,'$2','$3'}}. % (1)
pseudo -> pseudo_sym function_s_star '[' expr ']' ')' : {'$1',{function,'$2','$4'}}. % (5)

pseudo_sym -> ':'    : ':'.  % CSS2
pseudo_sym -> PSEUDO : '::'. % CSS3 (7)

validator_s_star -> validator        : '$1'.
validator_s_star -> validator s_plus : '$1'.

validator -> VALIDATOR : css_leex:value('$1').

ident_s_star -> ident        : '$1'.
ident_s_star -> ident s_plus : '$1'.

ident -> IDENT : css_leex:value('$1').

declaration -> property colon_s_star expr         : {':','$1','$3'}.
declaration -> property colon_s_star expr prio    : {':!important','$1','$3'}.
declaration -> property declaration_list          : {'{}','$1','$2'}. % (1)
declaration -> percentage_s_star declaration_list : {'{}','$1','$2'}. % (1)
declaration -> property equals_s_star expr        : {':','$1','$3'}. % (17)

declaration -> LENGTH        : bad4. % (4)
declaration -> LENGTH s_plus : bad4. % (4)

percentage_s_star -> PERCENTAGE        : css_leex:value('$1'). % (1)
percentage_s_star -> PERCENTAGE s_plus : css_leex:value('$1'). % (1)

prio -> IMPORTANT_SYM.
prio -> IMPORTANT_SYM s_plus.

expr -> expr1 : css_leex:flatten_expr('$1').

expr1 -> expr2                    : '$1'.
expr1 -> expr2 comma_s_star expr1 : {',','$1','$3'}.

expr2 -> expr3       : '$1'.
expr2 -> expr3 expr2 : {' ','$1','$2'}.

expr3 -> term                    : '$1'.
expr3 -> term slash_s_star expr3 : {'/','$1','$3'}.

term ->                term1        : '$1'.
term ->                term1 s_plus : '$1'.
term -> unary_operator term1        : css_leex:apply_unary_operator('$1', '$2').
term -> unary_operator term1 s_plus : css_leex:apply_unary_operator('$1', '$2').
term -> unary_operator s_plus term1        : css_leex:apply_unary_operator('$1', '$3').
term -> unary_operator s_plus term1 s_plus : css_leex:apply_unary_operator('$1', '$3').
term -> term2                       : '$1'.
term -> term2 s_plus                : '$1'.
term -> hexcolor                    : '$1'.
term -> hexcolor s_plus             : '$1'.
term -> function2                   : '$1'.
term -> function2 s_plus            : '$1'.

term1 -> NUMBER     : css_leex:value('$1').
term1 -> PERCENTAGE : css_leex:value('$1').
term1 -> LENGTH     : css_leex:value('$1').
term1 -> EMS        : css_leex:value('$1').
term1 -> EXS        : css_leex:value('$1').
term1 -> ANGLE      : css_leex:value('$1').
term1 -> TIME       : css_leex:value('$1').
term1 -> FREQ       : css_leex:value('$1').
term1 -> DIMENSION  : css_leex:value('$1').
term1 -> RESOLUTION : css_leex:value('$1').
term1 -> LUA        : {lua,css_leex:value('$1')}. % (18)

term2 -> string : '$1'.
term2 -> ident  : {ident,'$1'}.
term2 -> uri    : '$1'.
term2 -> pseudo_sym ident : {':','$2'}.
term2 -> '.' ident : {'.','$2'}. % (12)
term2 -> at_sym_s_star : {'@','$1'}. % (16)

string -> STRING : {string,css_leex:value('$1')}.

uri -> URI : {uri,css_leex:value('$1')}.

function -> function_s_star      cparen_s_star : {function,'$1',[]}.
function -> function_s_star expr cparen_s_star : {function,'$1','$2'}.

function_s_star -> FUNCTION        : css_leex:function_name('$1').
function_s_star -> FUNCTION s_plus : css_leex:function_name('$1').

%% (11)
function2 -> function2_s_star         cparen_s_star : {function,'$1',[]}.
function2 -> function2_s_star expr    cparen_s_star : {function,'$1','$2'}.
function2 -> function2_s_star kv_list cparen_s_star : {function,'$1',css_leex:flatten_expr('$2')}.

function2_s_star -> ident ':' function2_s_star : {ident,{':','$1','$3'}}.
function2_s_star -> ident '.' function2_s_star : {ident,{'.','$1','$3'}}.
function2_s_star -> FUNCTION        : css_leex:function_name('$1').
function2_s_star -> FUNCTION s_plus : css_leex:function_name('$1').

kv_list -> expr2 equals_s_star expr2                      : {'=','$1','$3'}.
kv_list -> expr2 equals_s_star expr2 comma_s_star kv_list : {',',{'=','$1','$3'},'$5'}.

equals_s_star -> '='        : '='.
equals_s_star -> '=' s_plus : '='.

hexcolor -> HASH        : css_leex:hash_to_hexcolor('$1').
hexcolor -> HASH s_plus : css_leex:hash_to_hexcolor('$1').

s_plus -> S.
s_plus -> S s_plus.


%% --- Grammar from [1], G.1. ---
%%
%% stylesheet
%%   : [ CHARSET_SYM STRING ';' ]?
%%     [S|CDO|CDC]* [ import [ CDO S* | CDC S* ]* ]*
%%     [ [ ruleset | media | page ] [ CDO S* | CDC S* ]* ]*
%%   ;
%% import
%%   : IMPORT_SYM S*
%%     [STRING|URI] S* media_list? ';' S*
%%   ;
%% media
%%   : MEDIA_SYM S* media_list '{' S* ruleset* '}' S*
%%   ;
%%
%% % -- begin CSS2 --
%% % media_list
%% %   : medium [ COMMA S* medium]*      % bug: COMMA -> ','
%% %   ;
%% % -- end CSS2 --
%%
%% % -- begin CSS3 --
%% media_query_list
%%  : S* [media_query [ ',' S* media_query ]* ]?
%%  ;
%% media_query
%%  : [ONLY | NOT]? S* media_type S* [ AND S* expression ]*
%%  | expression [ AND S* expression ]*
%%  ;
%% media_type
%%  : IDENT
%%  ;
%% expression
%%  : '(' S* media_feature S* [ ':' S* expr ]? ')' S*
%%  ;
%% media_feature
%%  : IDENT
%%  ;
%% % -- end CSS3 --
%%
%% medium
%%   : IDENT S*
%%   ;
%% page
%%   : PAGE_SYM S* pseudo_page?
%%     '{' S* declaration? [ ';' S* declaration? ]* '}' S*
%%   ;
%% pseudo_page
%%   : ':' IDENT S*
%%   ;
%% operator
%%   : '/' S* | ',' S*
%%   ;
%% combinator
%%   : '+' S*
%%   | '>' S*
%%   ;
%% unary_operator
%%   : '-' | '+'
%%   ;
%% property
%%   : IDENT S*
%%   ;
%% ruleset
%%   : selector [ ',' S* selector ]*
%%     '{' S* declaration? [ ';' S* declaration? ]* '}' S*
%%   ;
%% selector
%%   : simple_selector [ combinator selector | S+ [ combinator? selector ]? ]?
%%   ;
%% simple_selector
%%   : element_name [ HASH | class | attrib | pseudo ]*
%%   | [ HASH | class | attrib | pseudo ]+
%%   ;
%% class
%%   : '.' IDENT
%%   ;
%% element_name
%%   : IDENT | '*'
%%   ;
%% attrib
%%   : '[' S* IDENT S* [ [ '=' | INCLUDES | DASHMATCH ] S*
%%     [ IDENT | STRING ] S* ]? ']'
%%   ;
%% pseudo
%%   : ':' [ IDENT | FUNCTION S* [IDENT S*]? ')' ]
%%   ;
%% declaration
%%   : property ':' S* expr prio?
%%   ;
%% prio
%%   : IMPORTANT_SYM S*
%%   ;
%% expr
%%   : term [ operator? term ]*
%%   ;
%% term
%%   : unary_operator?
%%     [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* |
%%       TIME S* | FREQ S* ]
%%   | STRING S* | IDENT S* | URI S* | hexcolor | function
%%   ;
%% function
%%   : FUNCTION S* expr ')' S*
%%   ;
%% /*
%%  * There is a constraint on the color that it must
%%  * have either 3 or 6 hex-digits (i.e., [0-9a-fA-F])
%%  * after the "#"; e.g., "#000" is OK, but "#abcd" is not.
%%  */
%% hexcolor
%%   : HASH S*
%%   ;

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

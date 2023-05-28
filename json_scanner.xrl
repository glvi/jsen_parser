%%% JSON scanner for Erlang
%%%
%%% JSON scanner scans for the following patterns in the input:
%%%
%%%   - a literal "null", "false", or "true"; or
%%%   - a number.
%%%
%%% @end

%%% Note: When using regular expression variables, note that the
%%% variables are simply replaced by the text string that defines
%%% them.
%%%
%%% Given the following definitions
%%%
%%%   ABC = a|b|c
%%%   DEF = d|e|f
%%%
%%% then the following regular expressions
%%%
%%%   {ABC}{DEF}
%%%   {ABC}?
%%%   {ABC}*
%%%   {ABC}+
%%%
%%% are equivalent to
%%%
%%%   a|b|cd|e|f
%%%   a|b|c?
%%%   a|b|c*
%%%   a|b|c+
%%%
%%% but not equivalent to
%%%
%%%   (a|b|c)(d|e|f)
%%%   (a|b|c)?
%%%   (a|b|c)*
%%%   (a|b|c)+
%%%
%%% To be safe, use definitions in (parentheses) like so:
%%%
%%%   ({ABC})({DEF})
%%%   ({ABC})?
%%%   ({ABC})*
%%%   ({ABC})+
%%%
%%% (end note)

Definitions.

DEC  = [0-9]
DEC1 = [1-9]
HEX  = [0-9a-fA-F]
HEX1 = [1-9a-fA-F]

%% C0 control characters (U+0000...U+001F)
C0CTRL = \x00-\x1f

%% Whitespace
WS = \x09\x0a\x0d\x20

%% Quotation mark " (U+0022)
DQ = \x22

%% Reverse solidus \ (U+005C)
ESC = \x5c

%% Anything but the following:
%% - C0 controls, or
%% - quotation mark, or
%% - reverse solidus
CHAR = [^{C0CTRL}{DQ}{ESC}]

%% Escape sequences ("A->B" = "replace sequence A with B"):
%% - \" -> literal " (U+0022)
%% - \\ -> literal \ (U+005c)
%% - \/ -> literal / (U+002f)
%% - \b -> backspace (BS) (U+0008)
%% - \f -> form feed (FF) (U+000c)
%% - \n -> line feed (LF) (U+000a) a.k.a. new line (NL)
%% - \r -> carriage return (CR) (U+000d)
%% - \t -> character tabulation (HT) (U+0009) a.k.a. tab
%% - \uNNNN -> literal character (U+NNNN)
ESC_SEQ = {ESC}[{DQ}/bfnrt{ESC}]|{ESC}u{HEX}{HEX}{HEX}{HEX}

%% Numbers
POSITIVE = {DEC1}{DEC}*
NATURAL  = (0|{POSITIVE})
INTEGER  = \-?{NATURAL}
FRACTION = \.{NATURAL}
EXPONENT = [Ee][+-]?{NATURAL}

Rules.

{DQ}({CHAR}|{ESC_SEQ})*{DQ} :
  {token, {string, TokenLine, process(TokenChars)}}.

{INTEGER}{FRACTION}({EXPONENT})? :
  {token, {float, TokenLine, TokenChars}}.

{INTEGER} :
  {token, {int, TokenLine, TokenChars}}.

false : {token, {false, TokenLine}}.
true  : {token, {true , TokenLine}}.
null  : {token, {null , TokenLine}}.

\x2c : {token, {',', TokenLine}}.
\x3a : {token, {':', TokenLine}}.
\x5b : {token, {'[', TokenLine}}.
\x5d : {token, {']', TokenLine}}.
\x7b : {token, {'{', TokenLine}}.
\x7d : {token, {'}', TokenLine}}.

[{WS}]+ : skip_token.

Erlang code.

%%% @doc Trims the outer quotation marks from `String', and replaces
%%%      each occurrence of an escape sequence with its corresponding
%%%      codepoint.
process(String) ->
    S1 = string:trim(String, both, [$"]),
    escapes(S1).

%%% @doc Replaces each occurrence of an escape sequence with the
%%%      corresponding codepoint.
escapes(String) -> escapes("", String).

%%% Does the actual work for escapes/1.
escapes(ReversePrefix, []) ->
    lists:reverse(ReversePrefix);
escapes(ReversePrefix, [ $\\,$" | Suffix]) ->
    escapes([$" | ReversePrefix], Suffix);
escapes(ReversePrefix, [ $\\,$/ | Suffix]) ->
    escapes([$/ | ReversePrefix], Suffix);
escapes(ReversePrefix, [ $\\,$\\ | Suffix]) ->
    escapes([$\\ | ReversePrefix], Suffix);
escapes(ReversePrefix, [ $\\,$b | Suffix]) ->
    escapes([$\b | ReversePrefix], Suffix);
escapes(ReversePrefix, [ $\\,$f | Suffix]) ->
    escapes([$\f | ReversePrefix], Suffix);
escapes(ReversePrefix, [ $\\,$n | Suffix]) ->
    escapes([$\n | ReversePrefix], Suffix);
escapes(ReversePrefix, [ $\\,$r | Suffix]) ->
    escapes([$\r | ReversePrefix], Suffix);
escapes(ReversePrefix, [ $\\,$t | Suffix]) ->
    escapes([$\t | ReversePrefix], Suffix);
escapes(ReversePrefix, [ $\\,$u, A, B, C, D | Suffix]) ->
    escapes([codepoint(A,B,C,D) | ReversePrefix], Suffix);
escapes(ReversePrefix, [ Head | Suffix]) ->
    escapes([Head | ReversePrefix], Suffix).

%%% @doc Returns the codepoint denoted by four hexadecimal digits
codepoint(A,B,C,D) ->
        16#1000 * hex_value(A) +
        16#0100 * hex_value(B) +
        16#0010 * hex_value(C) +
        16#0001 * hex_value(D).

%%% @doc Returns the numerical value of `Int` interpreted as a
%%%      hexadecimal digit.
-spec hex_value(Int :: integer()) -> integer().

hex_value(Int) when $0 =< Int, Int =< $9 -> Int - $0;
hex_value(Int) when $A =< Int, Int =< $F -> Int - $A + 10;
hex_value(Int) when $a =< Int, Int =< $f -> Int - $a + 10;
hex_value(Int) -> error(badarg, [Int]).

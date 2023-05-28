%%% jsen_parser - JSON parser in Erlang.
%%% Copyright (C) 2023  GLVI Gesellschaft für Luftverkehrsinformatik mbH, Germany.
%%%
%%% This program is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see
%%% <https://www.gnu.org/licenses/>.
Nonterminals
  array json member members number object value values.

Terminals
  ',' ':' '[' ']' '{' '}' false float int null string true.

Rootsymbol
  json.

Endsymbol
  '$end'.

json -> value : '$1'.

value -> object : {object, lists:reverse('$1')}.
value -> array : {array, lists:reverse('$1')}.
value -> string : {_, _, String} = '$1', {string, String}.
value -> number : '$1'.
value -> true : true.
value -> false : false.
value -> null : null.

object -> '{' '}' : [].
object -> '{' members '}' : '$2'.

members -> member : ['$1'].
members -> members ',' member : ['$3'|'$1'].

member -> string ':' value :
  {_, _, String} = '$1',
  {String, '$3'}.

array -> '[' ']' : [].
array -> '[' values ']' : '$2'.

values -> value : ['$1'].
values -> values ',' value : ['$3'|'$1'].

number -> int :
   {_, _, Chars} = '$1',
   list_to_integer(Chars).

number -> float :
  {_, _, Chars} = '$1',
  list_to_float(Chars).

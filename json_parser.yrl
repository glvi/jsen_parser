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

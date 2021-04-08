-module(etc).

-export([lsubtract/2, normalize_permission/1]).

lsubtract(ValueSet0, ValueSet1) when is_list(ValueSet0) -> lsubtract(sets:from_list(ValueSet0), ValueSet1);
lsubtract(ValueSet0, ValueSet1) when is_list(ValueSet1) -> lsubtract(ValueSet0, sets:from_list(ValueSet1));
lsubtract(ValueSet0, ValueSet1) -> sets:to_list(sets:subtract(ValueSet0, ValueSet1)).

normalize_permission(Permission) when is_binary(Permission) ->
    normalize_permission(unicode:characters_to_list(Permission));
normalize_permission(Permission) when is_atom(Permission) -> normalize_permission(atom_to_list(Permission));
normalize_permission(Permission) when is_list(Permission) -> string:to_upper(Permission).
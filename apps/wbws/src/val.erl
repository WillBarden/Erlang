-module(val).

-compile(export_all).

len_range(VarName, Min) -> len_range(VarName, Min, infinity).

len_range(VarName, Min, Max) ->
    fun(Str) ->
        Len = length(unicode:characters_to_list(Str)),
        if
            Len < Min -> { invalid, io_lib:format("~s must be at least ~B characters long", [VarName, Min]) };
            Len > Max -> { invalid, io_lib:format("~s must be no longer than ~B characters", [VarName, Max]) };
            true -> valid
        end
    end.

alpha_limit(VarName, Min) -> alpha_limit(VarName, Min, infinity).

alpha_limit(VarName, Min, Max)->
    fun(Str) ->
        Count = item_count(lists:seq($a, $z) ++ lists:seq($A, $Z), unicode:characters_to_list(Str)),
        if
            Count < Min -> { invalid, io_lib:format("~s must contain at least ~B letters", [VarName, Min]) };
            Count > Max -> { invalid, io_lib:format("~s must contain no more than ~B letters", [VarName, Max]) };
            true -> valid
        end
    end.

num_limit(VarName, Min) -> num_limit(VarName, Min, infinity).

num_limit(VarName, Min, Max) ->
    fun(Str) ->
        Count = item_count(lists:seq($0, $9), unicode:characters_to_list(Str)),
        if
            Count < Min -> { invalid, io_lib:format("~s must contain at least ~B numbers", [VarName, Min]) };
            Count > Max -> { invalid, io_lib:format("~s must contain no more than ~B numbers", [VarName, Max]) };
            true -> valid
        end
    end.

sym_limit(VarName, Min) -> sym_limit(VarName, Min, infinity).

sym_limit(VarName, Min, Max) ->
    fun(Str) ->
        ValidSymbols = [$`, $~, $!, $@, $#, $$, $%, $^, $&, $*, $(, $), $-, $_, $+, $=]
            ++ [${, $}, $[, $], $\\, $|, $:, $;, $', $", $<, $>, $,, $., $/, $?],
        Count = item_count(ValidSymbols, unicode:characters_to_list(Str)),
        if
            Count < Min -> { invalid, io_lib:format("~s must contain at least ~B symbols", [VarName, Min]) };
            Count > Max -> { invalid, io_lib:format("~s must contain no more than ~B symbols", [VarName, Max]) };
            true -> valid
        end
    end.

item_count(Items, L) when is_list(Items), is_list(L) ->
    lists:foldl(
        fun(Elem, Count) ->
            case lists:member(Elem, Items) of
                true -> Count + 1;
                false -> Count
            end
        end, 
        0,
        L
    ).

validate(Value, []) -> valid;
validate(Value, [Validator | T]) -> case apply(Validator, [Value]) of
        valid -> validate(Value, T);
        { invalid, Reason } -> { invalid, Reason }
    end.

validate([]) -> valid;
validate([{ Value, Validators } | T]) -> case validate(Value, Validators) of
        valid -> validate(T);
        { invalid, Reason } -> { invalid, Reason }
    end.

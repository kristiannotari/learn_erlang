-module(caseof).
-export([
    add_to_list/2
]).

is_in_list(X, L) -> true.

add_to_list(X, L) ->
    case is_in_list(X, L) of
        true -> L;
        false -> [X|L]
    end.

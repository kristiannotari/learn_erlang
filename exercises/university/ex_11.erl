-module(ex_11).
-compile(export_all).

squared_int(L) ->
    [ round(math:pow(X,2)) || X <- L, is_integer(X)].

intersect(L1, L2) ->
    [ X || X <- L1, lists:member(X, L2)].

symmetric_difference(L1, L2) ->
    Intersection = intersect(L1, L2),
    [ X || X <- L1 ++ L2, not(lists:member(X, Intersection))].
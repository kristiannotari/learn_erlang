-module(ex_01).
% -export([
%     is_palindrome/1,
%     is_anagram/2
% ]).
-compile(export_all).
-define(SPECIAL_CHARS, [$., $,, $:, $;, $\s, $?, $!]).

is_palindrome(String) ->
    S = lists:filter(
        fun(E) -> not lists:member(E, ?SPECIAL_CHARS) end,
        String
    ),
    string:equal(S, string:reverse(S), true).

check_anagram(S1, S2) -> 
    S1_sorted = lists:sort(S1),
    S2_sorted = lists:sort(S2),
    S1_sorted =:= S2_sorted.

is_anagram(_String, []) -> false;
is_anagram(String, [H|TL]) ->
    case check_anagram(String, H) of 
        true -> true;
        false -> is_anagram(String, TL)
    end.

factors(Number) -> factors(Number, 2, []).
factors(Number, _D, _Acc) when Number < 1 -> [];
factors(1, _D, Acc) -> lists:reverse([1|Acc]);
factors(Number, D, Acc) ->
    case Number rem D of 
        0 -> factors(Number div D, D, [D|Acc]);
        _ -> factors(Number, D + 1, Acc)
    end.

is_proper(Number) -> 
    Number =:= lists:foldl(
        fun(E, Sum) -> Sum + E end,
        0,
        factors(Number) 
    ).
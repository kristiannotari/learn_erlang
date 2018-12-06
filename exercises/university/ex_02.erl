-module(ex_02).
-compile(export_all).

evaluate(Exp) ->
    case Exp of 
        {num, Value} -> Value;
        {Op, SubExp} -> 
            case Op of 
                negate -> -evaluate(SubExp)
            end;
        {Op, SubExpLeft, SubExpRight} ->
            case Op of 
                plus -> evaluate(SubExpLeft) + evaluate(SubExpRight);
                minus -> evaluate(SubExpLeft) - evaluate(SubExpRight)
            end
    end.

parse([]) -> {none};
parse(Exp) -> 
    case extract(Exp) of
        {binary, Op, Left, Right} -> {Op, parse(Left), parse(Right)};
        {unary, Op, Right} -> {Op, parse(Right)};
        {value, Value} -> {num, parse_integer(Value)};
        {error, Left, Message} -> throw({error, Message, length(Left)})
end.

parse_integer(Value) ->
    case string:to_integer(Value) of 
        {error, Reason} -> throw({error, Reason, Value});
        {Int, _Rest} -> Int 
    end.

extract(Exp) -> 
    extract(Exp, 0, []).
extract(_Exp, -1, Left) -> {error, Left, "Exp is wrongly bracketed"};
extract([$(|TL], 0, []) -> extract(TL, 1, []);
extract([$)|TL], 1, Left) -> extract(TL, 0, Left);
extract([$(|TL], Pairs, Left) -> extract(TL, Pairs + 1, Left ++ [$(]);
extract([$)|TL], Pairs, Left) -> extract(TL, Pairs - 1, Left ++ [$)]);
extract([H|TL], 0, []) ->
    case H of
        $~ -> {unary, negate, TL};
        _ -> extract(TL, 0, [H])
    end;
extract([H|TL], 1, Left) ->
    case H of
        $+ -> {binary, plus, Left, lists:sublist(TL, length(TL) - 1)};
        $- -> {binary, minus, Left, lists:sublist(TL, length(TL) - 1)};
        _ -> extract(TL, 1, Left ++ [H])
    end;
extract([H|TL], Pairs, Left) -> extract(TL, Pairs, Left ++ [H]);
extract([], Pairs, Left) -> 
    case Pairs of 
        0 -> {value, Left};
        _ -> {error, Left, "Exp is not fully bracketed"}
    end.

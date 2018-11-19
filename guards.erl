-module(guards).
-export([
    old_enough/1,
    can_drive/1
]).

old_enough(X) when X >= 18 -> true;
old_enough(_) -> false.

can_drive(X) when X >= 18, X < 100 -> true;
can_drive(_) -> false.
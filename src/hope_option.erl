-module(hope_option).


-export_type(
    [ t/1
    ]).

-export(
    [ put/2
    , get/2
    , map/2
    ]).


-type t(A) ::
      none
    | {some, A}
    .


put(X, F) ->
    case F(X)
    of  true  -> {some, X}
    ;   false -> none
    end.

get({some, X}, _) -> X;
get(none     , Y) -> Y.

map({some, X}, F) -> {some, F(X)};
map(none     , _) -> none.

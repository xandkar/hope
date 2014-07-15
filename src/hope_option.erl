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


-spec put(A, fun((A) -> boolean())) ->
    t(A).
put(X, F) ->
    case F(X)
    of  true  -> {some, X}
    ;   false -> none
    end.

-spec get(t(A), Default :: A) ->
    A.
get({some, X}, _) -> X;
get(none     , Y) -> Y.

-spec map(t(A), fun((A) -> (B))) ->
    t(B).
map({some, X}, F) -> {some, F(X)};
map(none     , _) -> none.

-module(hope_option).

-behavior(hope_monad).

-export_type(
    [ t/1
    ]).

-export(
    [ put/2
    , get/2
    , return/1
    , map/2
    , iter/2
    , pipe/2
    , of_result/1
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

-spec return(A) ->
    {some, A}.
return(X) ->
    {some, X}.

-spec map(t(A), fun((A) -> (B))) ->
    t(B).
map({some, X}, F) -> {some, F(X)};
map(none     , _) -> none.

-spec iter(t(A), fun((A) -> (ok))) ->
    ok.
iter({some, X}, F) -> ok = F(X);
iter(none     , _) -> ok.

-spec pipe([fun((A) -> t(B))], A) ->
    t(B).
pipe([], X) ->
    return(X);
pipe([F|Fs], X) ->
    case F(X)
    of  none      -> none
    ;   {some, Y} -> pipe(Fs, Y)
    end.

-spec of_result(hope_result:t(A, _B)) ->
    t(A).
of_result({ok, X})    -> {some, X};
of_result({error, _}) -> none.

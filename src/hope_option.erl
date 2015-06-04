-module(hope_option).

-behavior(hope_gen_monad).

-export_type(
    [ t/1
    ]).

-export(
    % Generic monad interface
    [ return/1
    , map/2
    , pipe/2

    % Specific to hope_option:t()
    , return/2
    , put/2
    , get/2
    , iter/2
    , of_result/1
    , of_undefined/1
    , validate/2
    ]).


-type t(A) ::
      none
    | {some, A}
    .


-spec put(A, fun((A) -> boolean())) ->
    t(A).
put(X, F) ->
    return(X, F).

-spec get(t(A), Default :: A) ->
    A.
get({some, X}, _) -> X;
get(none     , Y) -> Y.

-spec return(A) ->
    {some, A}.
return(X) ->
    {some, X}.

-spec return(A, fun((A) -> boolean())) ->
    t(A).
return(X, Condition) ->
    case Condition(X)
    of  true  -> {some, X}
    ;   false -> none
    end.

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

-spec of_undefined(undefined | A) ->
    t(A).
of_undefined(undefined) -> none;
of_undefined(X)         -> {some, X}.

-spec validate(t(A), fun((A) -> boolean())) ->
    t(A).
validate(none, _) ->
    none;
validate({some, X}=T, F) ->
    case F(X)
    of  false -> none
    ;   true  -> T
    end.

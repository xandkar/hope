-module(hope_fun).

-export(
    [ id/1
    , curry/1
    ]).

-spec id(A) ->
    A.
id(X) ->
    X.

-spec curry(fun()) ->
    fun().
curry(F) ->
    {some, Arity} = hope_kv_list:get(erlang:fun_info(F), arity),
    curry(F, [], Arity).

-spec curry(fun(), list(), integer()) ->
    fun().
curry(F, Args, 0) ->
    apply(F, lists:reverse(Args));
curry(F, Args, Arity) ->
    fun (X) -> curry(F, [X | Args], Arity - 1) end.

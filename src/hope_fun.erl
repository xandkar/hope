-module(hope_fun).

-export(
    [ id/1
    , curry/1
    , compose/1  % alias for compose_right/1
    , compose_right/1
    , compose_left/1
    , thread/2
    ]).

-spec id(A) ->
    A.
id(X) ->
    X.

-spec curry(fun()) ->
    fun().
curry(F) ->
    {arity, Arity} = erlang:fun_info(F, arity),
    curry(F, [], Arity).

-spec curry(fun(), list(), integer()) ->
    fun().
curry(F, Args, 0) ->
    apply(F, lists:reverse(Args));
curry(F, Args, Arity) ->
    fun (X) -> curry(F, [X | Args], Arity - 1) end.

-spec compose([fun((A) -> B)]) ->
    fun((A) -> B).
compose(Fs) ->
    compose_right(Fs).

-spec compose_right([fun((A) -> B)]) ->
    fun((A) -> B).
compose_right(Fs) ->
    compose_given_fold(Fs, fun lists:foldr/3).

-spec compose_left([fun((A) -> B)]) ->
    fun((A) -> B).
compose_left(Fs) ->
    compose_given_fold(Fs, fun lists:foldl/3).

-spec thread([fun((A) -> B)], A) ->
    B.
thread(Fs, X) ->
    F = compose_left(Fs),
    F(X).


%% ============================================================================

-spec compose_given_fold([fun((A) -> B)], fun((fun((A, B) -> C), B, [A]) -> C)) ->
    fun((A) -> C).
compose_given_fold(Fs, Fold) ->
    fun (X) -> Fold(fun (F, Y) -> F(Y) end, X, Fs) end.

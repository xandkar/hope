-module(hope_list).

-export_type(
    [ t/1
    ]).

-export(
    [ unique_preserve_order/1
    , map/2
    , map/3  % Tunable recursion limit
    , map_rev/2
    , map_slow/2
    , map_result/2  % Not tail-recursive
    , first_match/2
    ]).


-define(DEFAULT_RECURSION_LIMIT, 1000).


-type t(A) ::
    [A].


%% @doc Tail-recursive equivalent of lists:map/2
%% @end
-spec map([A], fun((A) -> (B))) ->
    [B].
map(Xs, F) ->
    map(Xs, F, ?DEFAULT_RECURSION_LIMIT).

-spec map([A], fun((A) -> (B)), RecursionLimit :: non_neg_integer()) ->
    [B].
map(Xs, F, RecursionLimit) ->
    map(Xs, F, RecursionLimit, 0).

map([], _, _, _) ->
    [];
map([X1], F, _, _) ->
    Y1 = F(X1),
    [Y1];
map([X1, X2], F, _, _) ->
    Y1 = F(X1),
    Y2 = F(X2),
    [Y1, Y2];
map([X1, X2, X3], F, _, _) ->
    Y1 = F(X1),
    Y2 = F(X2),
    Y3 = F(X3),
    [Y1, Y2, Y3];
map([X1, X2, X3, X4], F, _, _) ->
    Y1 = F(X1),
    Y2 = F(X2),
    Y3 = F(X3),
    Y4 = F(X4),
    [Y1, Y2, Y3, Y4];
map([X1, X2, X3, X4, X5 | Xs], F, RecursionLimit, RecursionCount) ->
    Y1 = F(X1),
    Y2 = F(X2),
    Y3 = F(X3),
    Y4 = F(X4),
    Y5 = F(X5),
    Ys =
        case RecursionCount > RecursionLimit
        of  true  -> map_slow(Xs, F)
        ;   false -> map     (Xs, F, RecursionLimit, RecursionCount + 1)
        end,
    [Y1, Y2, Y3, Y4, Y5 | Ys].

%% @doc lists:reverse(map_rev(L, F))
%% @end
-spec map_slow([A], fun((A) -> (B))) ->
    [B].
map_slow(Xs, F) ->
    lists:reverse(map_rev(Xs, F)).


%% @doc Tail-recursive alternative to lists:map/2, which accumulates and
%% returns list in reverse order.
%% @end
-spec map_rev([A], fun((A) -> (B))) ->
    [B].
map_rev(Xs, F) ->
    map_rev_acc(Xs, F, []).

-spec map_rev_acc([A], fun((A) -> (B)), [B]) ->
    [B].
map_rev_acc([], _, Ys) ->
    Ys;
map_rev_acc([X|Xs], F, Ys) ->
    Y = F(X),
    map_rev_acc(Xs, F, [Y|Ys]).

-spec map_result([A], fun((A) -> (hope_result:t(B, C)))) ->
    hope_result:t([B], C).
map_result([], _) ->
    {ok, []};
map_result([X | Xs], F) ->
    case F(X)
    of  {ok, Y} ->
            case map_result(Xs, F)
            of  {ok, Ys} ->
                    {ok, [Y | Ys]}
            ;   {error, _}=Error ->
                    Error
            end
    ;   {error, _}=Error ->
            Error
    end.

-spec unique_preserve_order(t(A)) ->
    t(A).
unique_preserve_order(L) ->
    PrependIfNew =
        fun (X, Xs) ->
            case lists:member(X, Xs)
            of  true  ->      Xs
            ;   false -> [X | Xs]
            end
        end,
    lists:reverse(lists:foldl(PrependIfNew, [], L)).

-spec first_match([{Tag, fun((A) -> boolean())}], A) ->
    hope_option:t(Tag).
first_match([], _) ->
    none;
first_match([{Tag, F} | Tests], X) ->
    case F(X)
    of  true  -> {some, Tag}
    ;   false -> first_match(Tests, X)
    end.

-module(hope_list).

-export_type(
    [ t/1
    ]).

-export(
    [ unique_preserve_order/1
    , map_rev/2
    , map_slow/2
    ]).


-type t(A) ::
    [A].


%% @doc lists:reverse(map_rev(L, F))
%% @end
-spec map_slow([A], fun((A) -> (B))) ->
    [B].
map_slow(Xs, F) ->
    lists:reverse(map_rev(Xs, F)).


%% @doc O(N), tail-recursive equivalent to lists:rev(lists:map(F, L))
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

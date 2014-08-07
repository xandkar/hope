-module(hope_list).

-export_type(
    [ t/1
    ]).

-export(
    [ unique_preserve_order/1
    ]).


-type t(A) ::
    [A].


-spec unique_preserve_order(t(A)) ->
    t(A).
unique_preserve_order(L) ->
    AppendIfNew =
        fun (X, Xs) ->
            case lists:member(X, Xs)
            of  true  -> Xs
            ;   false -> Xs ++ [X]
            end
        end,
    lists:foldl(AppendIfNew, [], L).

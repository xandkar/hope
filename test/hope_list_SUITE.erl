-module(hope_list_SUITE).

-include_lib("proper/include/proper.hrl").

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_unique_preserve_order/1
    , t_hope_list_specs/1
    , t_map_rev/1
    , t_map_slow/1
    , t_map/1
    , t_map_3/1
    , t_map_result/1
    ]).


-define(GROUP , hope_list).

-define(PROPTEST(A), true = proper:quickcheck(A())).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [{group, ?GROUP}].

groups() ->
    Tests =
        [ t_unique_preserve_order
        , t_hope_list_specs
        , t_map_rev
        , t_map_slow
        , t_map
        , t_map_3
        , t_map_result
        ],
    Properties = [parallel],
    [{?GROUP, Properties, Tests}].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_map_rev(_Cfg) ->
    ?PROPTEST(map_rev).

map_rev() ->
    ?FORALL({L, F}, {list(integer()), function([integer()], term())},
            hope_list:map_rev(L, F) == lists:reverse(lists:map(F, L))).

t_map_slow(_Cfg) ->
    ?PROPTEST(map_slow).

map_slow() ->
    ?FORALL({L, F}, {list(integer()), function([integer()], term())},
            hope_list:map_slow(L, F) == lists:map(F, L)).

t_map(_Cfg) ->
    ?PROPTEST(map).

map() ->
    ?FORALL({L, F}, {list(integer()), function([integer()], term())},
            hope_list:map(L, F) == lists:map(F, L)).

t_map_3(_Cfg) ->
    ?PROPTEST(map_3).

map_3() ->
    ?FORALL({L, F, N}, {list(integer()), function([integer()], term()), non_neg_integer()},
            hope_list:map(L, F, N) == lists:map(F, L)).

t_unique_preserve_order(_Cfg) ->
    ?PROPTEST(prop_unique_preserve_order).

prop_unique_preserve_order() ->
    ?FORALL(L, list(),
            begin
                Duplicates = L -- lists:usort(L),
                hope_list:unique_preserve_order(L) ==
                    lists:reverse(lists:reverse(L) -- Duplicates)
            end).

t_hope_list_specs(_) ->
    [] = proper:check_specs(hope_list).

t_map_result(_Cfg) ->
    AssertPositive =
        fun (I) when I > 0 -> {ok, I}; (_) -> {error, negative} end,
    AllPositives = lists:seq(1, 5),
    AllNegatives = lists:seq(-5, -1),
    Mixed = lists:seq(-5, 5),
    {ok, AllPositives} = hope_list:map_result(AllPositives, AssertPositive),
    {error, negative}  = hope_list:map_result(AllNegatives, AssertPositive),
    {error, negative}  = hope_list:map_result(Mixed, AssertPositive).

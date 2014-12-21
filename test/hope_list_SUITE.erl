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
        ],
    Properties = [parallel],
    [{?GROUP, Properties, Tests}].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_map_rev(_Cfg) ->
    F = fun (N) -> N + 1 end,
    [4, 3, 2] = hope_list:map_rev([1, 2, 3], F),
    []        = hope_list:map_rev([], F).

t_map_slow(_Cfg) ->
    F = fun (N) -> N + 1 end,
    [2, 3, 4] = hope_list:map_slow([1, 2, 3], F),
    []        = hope_list:map_slow([], F).

t_map(_Cfg) ->
    F = fun (N) -> N + 1 end,
    Xs = lists:seq(1, 5010),
    Ys = lists:map(F, Xs),
    Ys = hope_list:map(Xs, F),
    [] = hope_list:map([], F).

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

-module(hope_list_SUITE).

-include_lib("proper/include/proper_common.hrl").

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

-define(CHECK(F), true = proper:quickcheck(F())).

-define(type, proper_types).


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
    ?CHECK(proper_spec_map_rev).

t_map_slow(_Cfg) ->
    ?CHECK(proper_spec_map_slow).

t_map(_Cfg) ->
    ?CHECK(proper_spec_map).

t_map_3(_Cfg) ->
    ?CHECK(proper_spec_map_3).

t_unique_preserve_order(_Cfg) ->
    ?CHECK(proper_spec_prop_unique_preserve_order).

t_hope_list_specs(_Cfg) ->
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

%% ============================================================================
%% PropEr test specs
%% ============================================================================

proper_spec_map_rev() ->
    ?FORALL({L, F}, {type_l(), type_f()},
        hope_list:map_rev(L, F) == lists:reverse(lists:map(F, L))
    ).

proper_spec_map_slow() ->
    ?FORALL({L, F}, {type_l(), type_f()},
        hope_list:map_slow(L, F) == lists:map(F, L)
    ).

proper_spec_map() ->
    ?FORALL({L, F}, {type_l(), type_f()},
        hope_list:map(L, F) == lists:map(F, L)
    ).

proper_spec_map_3() ->
    ?FORALL({L, F, N}, {type_l(), type_f(), ?type:non_neg_integer()},
        hope_list:map(L, F, N) == lists:map(F, L)
    ).

proper_spec_prop_unique_preserve_order() ->
    ?FORALL(L, ?type:list(),
    begin
        Duplicates = L -- lists:usort(L),
        hope_list:unique_preserve_order(L) ==
            lists:reverse(lists:reverse(L) -- Duplicates)
    end).

type_l() ->
    ?type:list(?type:integer()).

type_f() ->
    ?type:function([?type:integer()], ?type:term()).

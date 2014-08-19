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
        ],
    Properties = [],
    [{?GROUP, Properties, Tests}].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_unique_preserve_order(_Cfg) ->
    ?PROPTEST(prop_unique_preserve_order).

prop_unique_preserve_order() ->
    ?FORALL(L, list(),
            begin
                Duplicates = L -- lists:usort(L),
                hope_list:unique_preserve_order(L) ==
                    lists:reverse(lists:reverse(L) -- Duplicates)
            end).

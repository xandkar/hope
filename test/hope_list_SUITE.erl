-module(hope_list_SUITE).

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
    ListAGiven = [a, a, g, b, f, c, a, d, a, e, f, d],
    ListBGiven = "australia",
    ListAExpected = [a, g, b, f, c, d, e],
    ListBExpected = "austrli",
    ListAComputed = hope_list:unique_preserve_order(ListAGiven),
    ListBComputed = hope_list:unique_preserve_order(ListBGiven),
    ListAComputed = ListAExpected,
    ListBComputed = ListBExpected.

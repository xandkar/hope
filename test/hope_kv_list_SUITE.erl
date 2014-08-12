-module(hope_kv_list_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [
    ]).


-define(GROUP_KV_LIST, kv_list).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [{group, ?GROUP_KV_LIST}].

groups() ->
    Tests =
        [
        ],
    Properties = [],
    [{?GROUP_KV_LIST, Properties, Tests}].


%% =============================================================================
%%  Test cases
%% =============================================================================

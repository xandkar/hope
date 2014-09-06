-module(hope_kv_list_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_validate_unique_presence/1
    ]).


-define(GROUP , hope_kv_list).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [{group, ?GROUP}].

groups() ->
    Tests =
        [ t_validate_unique_presence
        ],
    Properties = [],
    [{?GROUP, Properties, Tests}].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_validate_unique_presence(_Cfg) ->
    KeysRequired = [a, b, c],
    DictOk      = [{a, 1}, {b, 2}, {c, 3}],
    DictUnsup   = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],
    DictDups    = [{a, 1}, {b, 2}, {c, 3}, {a, 4}],
    DictMissing = [{a, 1}, {b, 2}],
    {ok, ok} =
        hope_kv_list:validate_unique_presence(DictOk, KeysRequired),
    {error, [{keys_unsupported, [d]}]} =
        hope_kv_list:validate_unique_presence(DictUnsup, KeysRequired),
    {error, [{keys_duplicated, [a]}]} =
        hope_kv_list:validate_unique_presence(DictDups, KeysRequired),
    {error, [{keys_missing, [c]}]} =
        hope_kv_list:validate_unique_presence(DictMissing, KeysRequired).

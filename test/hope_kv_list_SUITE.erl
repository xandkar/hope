-module(hope_kv_list_SUITE).

-include_lib("hope_kv_list.hrl").

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

    {ok, DictOk} =
        hope_kv_list:validate_unique_presence(DictOk, KeysRequired),
    #hope_kv_list_presence_violations
    { keys_missing     = []
    , keys_duplicated  = []
    , keys_unsupported = []
    } =
        hope_kv_list:find_unique_presence_violations(DictOk, KeysRequired),

    {error, [{keys_unsupported, [d]}]} =
        hope_kv_list:validate_unique_presence(DictUnsup, KeysRequired),
    #hope_kv_list_presence_violations
    { keys_missing     = []
    , keys_duplicated  = []
    , keys_unsupported = [d]
    } =
        hope_kv_list:find_unique_presence_violations(DictUnsup, KeysRequired),

    {error, [{keys_duplicated, [a]}]} =
        hope_kv_list:validate_unique_presence(DictDups, KeysRequired),
    #hope_kv_list_presence_violations
    { keys_missing     = []
    , keys_duplicated  = [a]
    , keys_unsupported = []
    } =
        hope_kv_list:find_unique_presence_violations(DictDups, KeysRequired),

    {error, [{keys_missing, [c]}]} =
        hope_kv_list:validate_unique_presence(DictMissing, KeysRequired),
    #hope_kv_list_presence_violations
    { keys_missing     = [c]
    , keys_duplicated  = []
    , keys_unsupported = []
    } =
        hope_kv_list:find_unique_presence_violations(DictMissing, KeysRequired).

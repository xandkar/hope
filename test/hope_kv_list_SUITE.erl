-module(hope_kv_list_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_set_new/1
    , t_set_existing/1
    , t_pop/1
    ]).


-define(GROUP_KV_LIST, kv_list).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

%% TODO: Make tests generic for any dictionary.
%% TODO: Each group should test a type of dictionary against the generic cases.

all() ->
    [{group, ?GROUP_KV_LIST}].

groups() ->
    Tests =
        [ t_set_new
        , t_set_existing
        , t_pop
        ],
    Properties = [],
    [{?GROUP_KV_LIST, Properties, Tests}].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_set_new(_Config) ->
    Key           = key,
    ValExpected   = bar,
    ListInitial   = hope_kv_list:empty(),
    ListResulting = hope_kv_list:set(ListInitial, Key, ValExpected),
    {some, ValResulting} = hope_kv_list:get(ListResulting, Key),
    ValResulting = ValExpected.

t_set_existing(_Config) ->
    Key           = key,
    ValInitial    = foo,
    ValExpected   = bar,
    ListInitial   = [{donald, duck}, {Key, ValInitial}],
    ListResulting = hope_kv_list:set(ListInitial, Key, ValExpected),
    {some, ValResulting} = hope_kv_list:get(ListResulting, Key),
    ValResulting = ValExpected.

t_pop(_Config) ->
    KVList = [{a, 1}, {b, 2}, {c, 3}],
    Dict1 = hope_kv_list:of_kv_list(KVList),
    {{some, 1},  Dict2} = hope_kv_list:pop(Dict1, a),
    {none     , _Dict3} = hope_kv_list:pop(Dict2, a).

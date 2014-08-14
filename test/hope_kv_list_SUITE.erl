-module(hope_kv_list_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    , init_per_group/2
    , end_per_group/2
    ]).

%% Test cases
-export(
    [ t_set_new/1
    , t_set_existing/1
    , t_pop/1
    ]).


-define(DICT_MODULE         , dict_module).
-define(DICT_MODULE_KV_LIST , hope_kv_list).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [{group, ?DICT_MODULE_KV_LIST}].

groups() ->
    Tests =
        [ t_set_new
        , t_set_existing
        , t_pop
        ],
    Properties = [],
    [{?DICT_MODULE_KV_LIST, Properties, Tests}].

init_per_group(DictModule, Cfg) ->
    hope_kv_list:set(Cfg, ?DICT_MODULE, DictModule).

end_per_group(_DictModule, _Cfg) ->
    ok.


%% =============================================================================
%%  Test cases
%% =============================================================================

t_set_new(Cfg) ->
    {some, DictModule} = hope_kv_list:get(Cfg, ?DICT_MODULE),
    Key           = key,
    ValExpected   = bar,
    ListInitial   = DictModule:empty(),
    ListResulting = DictModule:set(ListInitial, Key, ValExpected),
    {some, ValResulting} = DictModule:get(ListResulting, Key),
    ValResulting = ValExpected.

t_set_existing(Cfg) ->
    {some, DictModule} = hope_kv_list:get(Cfg, ?DICT_MODULE),
    Key           = key,
    ValInitial    = foo,
    ValExpected   = bar,
    ListInitial   = [{donald, duck}, {Key, ValInitial}],
    ListResulting = DictModule:set(ListInitial, Key, ValExpected),
    {some, ValResulting} = DictModule:get(ListResulting, Key),
    ValResulting = ValExpected.

t_pop(Cfg) ->
    {some, DictModule} = hope_kv_list:get(Cfg, ?DICT_MODULE),
    KVList = [{a, 1}, {b, 2}, {c, 3}],
    Dict1 = DictModule:of_kv_list(KVList),
    {{some, 1} , Dict2} = DictModule:pop(Dict1, a),
    {none      , Dict3} = DictModule:pop(Dict2, a),
    {{some, 2} , Dict4} = DictModule:pop(Dict3, b),
    {none      , Dict5} = DictModule:pop(Dict4, b),
    {{some, 3} , Dict6} = DictModule:pop(Dict5, c),
    {none      , Dict7} = DictModule:pop(Dict6, c),
    [] = DictModule:to_kv_list(Dict7).

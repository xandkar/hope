-module(hope_dictionary_SUITE).

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
    , t_get/1
    , t_pop/1
    , t_fold/1
    , t_dictionary_specs/1
    , t_has_key/1
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
        , t_get
        , t_pop
        , t_fold

        % TODO: Find-out why t_dictionary_specs failes with latest proper HEAD:
        %
        %   Testing hope_kv_list:to_kv_list/1
        %   Error: The typeserver encountered an error: {unbound_var,'K'}.
        %   *** CT Error Notification 2015-09-26 13:46:38.684 ***
        %   hope_dictionary_SUITE:t_dictionary_specs failed on line 111
        %   Reason: {badmatch,[{hope_kv_list,of_kv_list,1}]}
        %
        %, t_dictionary_specs

        , t_has_key
        ],
    Properties = [parallel],
    [{?DICT_MODULE_KV_LIST, Properties, Tests}].

init_per_group(DictModule, Cfg) ->
    hope_kv_list:set(Cfg, ?DICT_MODULE, DictModule).

end_per_group(_DictModule, _Cfg) ->
    ok.


%% =============================================================================
%%  Test cases
%% =============================================================================

t_get(Cfg) ->
    {some, DictModule} = hope_kv_list:get(Cfg, ?DICT_MODULE),
    K1 = k1,
    K2 = k2,
    V1 = v1,
    V2 = v2,
    D = DictModule:set(DictModule:empty(), K1, V1),
    {some, V1} = DictModule:get(D, K1),
    V1         = DictModule:get(D, K1, V2),
    none       = DictModule:get(D, K2),
    V2         = DictModule:get(D, K2, V2),
    default    = DictModule:get(D, K1, default, fun (X) -> X =:= foo end),
    V1         = DictModule:get(D, K1, default, fun (X) -> X =:= V1 end).

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

t_fold(Cfg) ->
    {some, DictModule} = hope_kv_list:get(Cfg, ?DICT_MODULE),
    KVList = [{a, 1}, {a, 5}, {b, 3}, {c, 4}, {c, 4}],
    Dict = DictModule:of_kv_list(KVList),
    17 = DictModule:fold(Dict, fun (_K, V, Acc) -> V + Acc end, 0).

t_dictionary_specs(Cfg) ->
    {some, DictModule} = hope_kv_list:get(Cfg, ?DICT_MODULE),
    [] = proper:check_specs(DictModule).

t_has_key(Cfg) ->
    {some, DictModule} = hope_kv_list:get(Cfg, ?DICT_MODULE),
    D = DictModule:of_kv_list([{a, 1}, {b, 2}, {c, 3}]),
    true  = DictModule:has_key(D, a),
    true  = DictModule:has_key(D, b),
    true  = DictModule:has_key(D, c),
    false = DictModule:has_key(D, d),
    false = DictModule:has_key(D, e),
    false = DictModule:has_key(D, f).

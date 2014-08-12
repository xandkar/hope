%%%----------------------------------------------------------------------------
%%% Equivalent to stdlib's orddict, but with a pretty (IMO), uniform interface.
%%%----------------------------------------------------------------------------
-module(hope_kv_list).

-behavior(hope_dictionary).

-export_type(
    [ t/2
    ]).

-export(
    [ empty/0
    , get/2
    , set/3
    , update/3
    , iter/2
    , map/2
    , filter/2
    , fold/3
    , of_kv_list/1
    , to_kv_list/1
    ]).


-type t(K, V) ::
    [{K, V}].


%% ============================================================================
%% API
%% ============================================================================

-spec empty() ->
    [].
empty() ->
    [].

get(T, K) ->
    case lists:keyfind(K, 1, T)
    of  false  -> none
    ;   {K, V} -> {some, V}
    end.

set(T, K, V) ->
    lists:keystore(K, 1, T, {K, V}).

update(T, K, F) ->
    V1Opt = get(T, K),
    V2 = F(V1Opt),
    set(T, K, V2).

iter(T, Map1) ->
    Map2 = lift_map_into_list(Map1),
    lists:foreach(Map2, T).

map(T, Map1) ->
    Map2 = lift_map_into_list(Map1),
    lists:map(Map2, T).

filter(T, Map1) ->
    Map2 = lift_map_into_list(Map1),
    lists:filter(Map2, T).

fold(T, F1, Accumulator) ->
    F2 = fun ({K, V}, Acc) -> F1(K, V, Acc) end,
    lists:foldl(F2, T, Accumulator).

to_kv_list(T) ->
    T.

of_kv_list(List) ->
    List.


%% ============================================================================
%% Helpers
%% ============================================================================

lift_map_into_list(Map) ->
    fun ({K, V}) -> {K, Map(K, V)} end.

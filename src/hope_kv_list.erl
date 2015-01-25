%%%----------------------------------------------------------------------------
%%% Equivalent to stdlib's orddict, but with a pretty (IMO), uniform interface.
%%%----------------------------------------------------------------------------
-module(hope_kv_list).

-behavior(hope_gen_dictionary).

-export_type(
    [ t/2
    ]).

-export(
    [ empty/0
    , get/2
    , get/3
    , set/3
    , update/3
    , pop/2
    , iter/2
    , map/2
    , filter/2
    , fold/3
    , of_kv_list/1
    , to_kv_list/1
    , validate_unique_presence/2  % Assume default optional parameter(s)
    , validate_unique_presence/3  % Specify optional parameter(s)
    ]).


-type t(K, V) ::
    [{K, V}].

-type presence_error(A) ::
      {keys_missing     , [A]}
    | {keys_duplicated  , [A]}
    | {keys_unsupported , [A]}
    .


%% ============================================================================
%% API
%% ============================================================================

-spec empty() ->
    [].
empty() ->
    [].

-spec get(t(K, V), K) ->
    hope_option:t(V).
get(T, K) ->
    case lists:keyfind(K, 1, T)
    of  false  -> none
    ;   {K, V} -> {some, V}
    end.

-spec get(t(K, V), K, V) ->
    V.
get(T, K, Default) ->
    Vopt = get(T, K),
    hope_option:get(Vopt, Default).

-spec set(t(K, V), K, V) ->
    t(K, V).
set(T, K, V) ->
    lists:keystore(K, 1, T, {K, V}).

-spec update(t(K, V), K, fun((hope_option:t(V)) -> V)) ->
    t(K, V).
update(T, K, F) ->
    V1Opt = get(T, K),
    V2 = F(V1Opt),
    % TODO: Eliminate the 2nd lookup.
    set(T, K, V2).

-spec pop(t(K, V), K) ->
    {hope_option:t(V), t(K, V)}.
pop(T1, K) ->
    case lists:keytake(K, 1, T1)
    of  {value, {K, V}, T2} -> {{some, V}, T2}
    ;   false               -> {none     , T1}
    end.

-spec iter(t(K, V), fun((K, V) -> ok)) ->
    ok.
iter(T, F1) ->
    F2 = lift_map(F1),
    lists:foreach(F2, T).

-spec map(t(K, V), fun((K, V) -> V)) ->
    t(K, V).
map(T, F1) ->
    F2 = fun ({K, _}=X) -> {K, apply_map(F1, X)} end,
    lists:map(F2, T).

-spec filter(t(K, V), fun((K, V) -> boolean())) ->
    t(K, V).
filter(T, F1) ->
    F2 = lift_map(F1),
    lists:filter(F2, T).

-spec fold(t(K, V), fun((K, V, Acc) -> Acc), Acc) ->
    Acc.
fold(T, F1, Accumulator) ->
    F2 = fun ({K, V}, Acc) -> F1(K, V, Acc) end,
    lists:foldl(F2, Accumulator, T).

-spec to_kv_list(t(K, V)) ->
    [{K, V}].
to_kv_list(T) ->
    T.

-spec of_kv_list([{K, V}]) ->
    t(K, V).
of_kv_list(List) ->
    % TODO: Decide if validation is to be done here. Do so if yes.
    List.

-spec validate_unique_presence(t(K, _V), [K]) ->
    hope_result:t(ok, [presence_error(K)]).
validate_unique_presence(T, KeysRequired) ->
    KeysOptional = [],
    validate_unique_presence(T, KeysRequired, KeysOptional).

-spec validate_unique_presence(t(K, _V), [K], [K]) ->
    hope_result:t(ok, [presence_error(K)]).
validate_unique_presence(T, KeysRequired, KeysOptional) ->
    KeysSupported   = KeysRequired ++ KeysOptional,
    KeysGiven       = [K || {K, _V} <- T],
    KeysGivenUnique = lists:usort(KeysGiven),
    KeysDups        = lists:usort(KeysGiven -- KeysGivenUnique),
    KeysMissing     = KeysRequired -- KeysGivenUnique,
    KeysUnsupported = KeysGivenUnique -- KeysSupported,
    case {KeysDups, KeysMissing, KeysUnsupported}
    of  {[], [], []} ->
            {ok, ok}
    ;   {Dups, Missing, Unsupported} ->
            ErrorDups =
                case Dups
                of  []    -> []
                ;   [_|_] -> [{keys_duplicated, Dups}]
                end,
            ErrorMissing =
                case Missing
                of  []    -> []
                ;   [_|_] -> [{keys_missing, Missing}]
                end,
            ErrorUnsupported =
                case Unsupported
                of  []    -> []
                ;   [_|_] -> [{keys_unsupported, Unsupported}]
                end,
            Errors = ErrorDups ++ ErrorMissing ++ ErrorUnsupported,
            {error, Errors}
    end.


%% ============================================================================
%% Helpers
%% ============================================================================

-spec lift_map(F) ->
    G
    when F :: fun(( K, V1 ) -> V2)
       , G :: fun(({K, V1}) -> V2)
       .
lift_map(F) ->
    fun (X) -> apply_map(F, X) end.

-spec apply_map(fun((K, V1) -> V2), {K, V1}) ->
    V2.
apply_map(F, {K, V}) ->
    F(K, V).

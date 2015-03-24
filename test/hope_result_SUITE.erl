-module(hope_result_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    , init_per_group/2
    , end_per_group/2
    ]).

%% Test cases
-export(
    [ t_pipe_ok/1
    , t_pipe_error/1
    , t_hope_result_specs/1
    , t_lift_exn/1
    , t_lift_map_exn/1
    , t_return/1
    , t_map/1
    , t_map_error/1
    , t_tag_error/1
    ]).


-define(GROUP_PIPE, result_pipe).
-define(GROUP_SPEC, result_spec).
-define(GROUP_LIFT, result_lift_exn).
-define(GROUP_OTHER, result_other).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [ {group, ?GROUP_PIPE}
    , {group, ?GROUP_SPEC}
    , {group, ?GROUP_LIFT}
    , {group, ?GROUP_OTHER}
    ].

groups() ->
    PipeTests =
        [ t_pipe_ok
        , t_pipe_error
        ],
    SpecTests =
        [ t_hope_result_specs
        ],
    LiftTests =
        [ t_lift_exn
        , t_lift_map_exn
        ],
    OtherTests =
        [ t_return
        , t_map
        , t_map_error
        , t_tag_error
        ],
    Properties = [parallel],
    [ {?GROUP_PIPE, Properties, PipeTests}
    , {?GROUP_SPEC, Properties, SpecTests}
    , {?GROUP_LIFT, Properties, LiftTests}
    , {?GROUP_OTHER, Properties, OtherTests}
    ].

init_per_group(?GROUP_PIPE, Cfg) ->
    Steps =
        [ fun (0) -> {ok, 1}; (X) -> {error, X} end
        , fun (1) -> {ok, 2}; (X) -> {error, X} end
        , fun (2) -> {ok, 3}; (X) -> {error, X} end
        ],
    hope_kv_list:set(Cfg, steps, Steps);
init_per_group(_, Cfg) ->
    Cfg.

end_per_group(_, _Cfg) ->
    ok.


%% =============================================================================
%%  Test cases
%% =============================================================================

t_pipe_ok(Cfg) ->
    {some, Steps} = hope_kv_list:get(Cfg, steps),
    {ok, 3} = hope_result:pipe(Steps, 0).

t_pipe_error(Cfg) ->
    {some, Steps} = hope_kv_list:get(Cfg, steps),
    {error, 1} = hope_result:pipe(Steps, 1).

t_hope_result_specs(_) ->
    [] = proper:check_specs(hope_result).

t_lift_exn(_Cfg) ->
    Class = throw,
    Reason = foofoo,
    Label = bar,
    F = fun (ok) -> apply(erlang, Class, [Reason]) end,
    G = hope_result:lift_exn(F),
    H = hope_result:lift_exn(F, Label),
    {error, {Class, Reason}} = G(ok),
    {error, {Label, {Class, Reason}}} = H(ok).

t_lift_map_exn(_Cfg) ->
    FOk  = fun ({}) -> foo end,
    FExn = fun ({}) -> throw(baz) end,
    MapOk          = fun (foo)          -> bar end,
    MapOkThrows    = fun (foo)          -> throw(exn_from_ok_map) end,
    MapError       = fun ({throw, baz}) -> qux end,
    MapErrorThrows = fun ({throw, baz}) -> throw(exn_from_error_map) end,
    GOk          = hope_result:lift_map_exn(FOk , MapOk      , MapError),
    GOkThrows    = hope_result:lift_map_exn(FOk , MapOkThrows, MapError),
    GError       = hope_result:lift_map_exn(FExn, MapOk      , MapError),
    GErrorThrows = hope_result:lift_map_exn(FExn, MapOk      , MapErrorThrows),
    {ok, bar} = GOk({}),
    {error, qux} = GError({}),
    ok =
        try
            must_not_return = GOkThrows({})
        catch throw:exn_from_ok_map ->
                ok
        end,
    ok =
        try
            must_not_return = GErrorThrows({})
        catch throw:exn_from_error_map ->
                ok
        end.

t_return(_Cfg) ->
    X = foo,
    {ok, X} = hope_result:return(X).

t_map(_Cfg) ->
    X = foo,
    Y = bar,
    F = fun (foo) -> Y end,
    {ok, Y}    = hope_result:map({ok, X}, F),
    {error, X} = hope_result:map({error, X}, F).

t_map_error(_Cfg) ->
    X = foo,
    Y = bar,
    XtoY = fun (foo) -> Y end,
    {ok   , X} = hope_result:map_error({ok   , X}, XtoY),
    {error, Y} = hope_result:map_error({error, X}, XtoY).

t_tag_error(_Cfg) ->
    X = foo,
    Tag = bar,
    {ok   ,       X } = hope_result:tag_error({ok   , X}, Tag),
    {error, {Tag, X}} = hope_result:tag_error({error, X}, Tag).

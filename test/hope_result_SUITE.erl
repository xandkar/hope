-module(hope_result_SUITE).

%% TODO: Import only what is used.
-include_lib("proper/include/proper.hrl").

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
    ]).


-define(GROUP_PIPE, result_pipe).
-define(GROUP_SPEC, result_spec).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [ {group, ?GROUP_PIPE}
    , {group, ?GROUP_SPEC}
    ].

groups() ->
    PipeTests =
        [ t_pipe_ok
        , t_pipe_error
        ],
    SpecTests =
        [ t_hope_result_specs
        ],
    Properties = [parallel],
    [ {?GROUP_PIPE, Properties, PipeTests}
    , {?GROUP_SPEC, Properties, SpecTests}
    ].

init_per_group(?GROUP_SPEC, Cfg) ->
    Cfg;
init_per_group(?GROUP_PIPE, Cfg) ->
    Steps =
        [ fun (0) -> {ok, 1}; (X) -> {error, X} end
        , fun (1) -> {ok, 2}; (X) -> {error, X} end
        , fun (2) -> {ok, 3}; (X) -> {error, X} end
        ],
    hope_kv_list:set(Cfg, steps, Steps).

end_per_group(?GROUP_SPEC, _Cfg) ->
    ok;
end_per_group(?GROUP_PIPE, _Cfg) ->
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

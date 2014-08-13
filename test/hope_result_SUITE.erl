-module(hope_result_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_pipe/1
    ]).


-define(GROUP_PIPE, result_pipe).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [{group, ?GROUP_PIPE}].

groups() ->
    Tests =
        [ t_pipe
        ],
    Properties = [],
    [{?GROUP_PIPE, Properties, Tests}].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_pipe(_Config) ->
    Steps =
        [ fun (0) -> {ok, 1}; (X) -> {error, X} end
        , fun (1) -> {ok, 2}; (X) -> {error, X} end
        , fun (2) -> {ok, 3}; (X) -> {error, X} end
        ],
    {ok, 3} = hope_result:pipe(Steps, 0).

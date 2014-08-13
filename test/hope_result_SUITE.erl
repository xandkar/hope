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
    A = foo,
    Z = qux,
    Steps =
        [ fun (foo) -> {ok, bar}; (X) -> {error, X} end
        , fun (bar) -> {ok, baz}; (X) -> {error, X} end
        , fun (baz) -> {ok, qux}; (X) -> {error, X} end
        ],
    {ok, Z} = hope_result:pipe(Steps, A).

-module(hope_fun_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_id/1
    ]).


-define(GROUP, hope_fun).


%% ============================================================================
%% Common Test callbacks
%% ============================================================================

all() ->
    [ {group, ?GROUP}
    ].

groups() ->
    Tests =
        [ t_id
        ],
    Properties = [parallel],
    [ {?GROUP, Properties, Tests}
    ].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_id(_Cfg) ->
    X = foo,
    X = hope_fun:id(X).

-module(hope_fun_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_id/1
    , t_curry/1
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
        , t_curry
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

t_curry(_Cfg) ->
    Single = fun (X) -> X end,
    Double = fun (X, Y) -> {X, Y} end,
    Triple = fun (X, Y, Z) -> {X, Y, Z} end,

    F = hope_fun:curry(Single),
    a = F(a),

    G1 = hope_fun:curry(Double),
    G = G1(a),
    {a, b} = G(b),

    H1 = hope_fun:curry(Triple),
    H2 = H1(a),
    H  = H2(b),
    {a, b, c} = H(c).

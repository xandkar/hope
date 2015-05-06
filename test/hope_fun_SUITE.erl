-module(hope_fun_SUITE).

%% Callbacks
-export(
    [ all/0
    , groups/0
    ]).

%% Test cases
-export(
    [ t_specs/1
    , t_id/1
    , t_curry/1
    , t_compose_and_thread/1
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
        [ t_specs
        , t_id
        , t_curry
        , t_compose_and_thread
        ],
    Properties = [parallel],
    [ {?GROUP, Properties, Tests}
    ].


%% =============================================================================
%%  Test cases
%% =============================================================================

t_specs(_) ->
    [] = proper:check_specs(hope_fun).

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

t_compose_and_thread(_Cfg) ->
    A2B = fun (a) -> b end,
    B2C = fun (b) -> c end,
    C2D = fun (c) -> d end,
    Fs = [C2D, B2C, A2B],
    d = (hope_fun:compose       (              Fs  ))(a),
    d = (hope_fun:compose_right (              Fs  ))(a),
    d = (hope_fun:compose_left  (lists:reverse(Fs) ))(a),
    d =  hope_fun:thread        (lists:reverse(Fs),   a).

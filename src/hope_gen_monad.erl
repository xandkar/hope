-module(hope_gen_monad).

-type t(_A) ::
    term().

-callback return(A) ->
    t(A).

-callback map(t(A), fun((A) -> (B))) ->
    t(B).

%% @doc "pipe" is equivalent to traditional "bind", in general use-case, but is
%% arguably more useful for composition in Erlang's syntactic setting.
%% @end
-callback pipe([fun((A) -> t(B))], A) ->
    t(B).

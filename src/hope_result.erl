-module(hope_result).


-export_type(
    [ t/2
    ]).

-export(
    [ pipe/2
    ]).


-type t(A, B) ::
      {ok, A}
    | {error, B}
    .


-spec pipe([F], X) ->
    t(Ok, Error)
    when X     :: any()
       , Ok    :: any()
       , Error :: any()
       , F     :: fun((X) -> t(Ok, Error))
       .
pipe([]    , X) -> X;
pipe([F|Fs], X) ->
    case F(X)
    of  {error, _}=E -> E
    ;   {ok, Y}      -> pipe(Fs, Y)
    end.

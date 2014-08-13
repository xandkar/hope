-module(hope_result).


-export_type(
    [ t/2
    ]).

-export(
    [ pipe/2
    , lift_exn/1
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
pipe([], X) ->
    {ok, X};
pipe([F|Fs], X) ->
    case F(X)
    of  {error, _}=E -> E
    ;   {ok, Y}      -> pipe(Fs, Y)
    end.

-spec lift_exn(F) -> G
    when F     :: fun((A)-> B)
       , G     :: fun((A)-> t(B, {Class, Reason :: any()}))
       , Class :: error
                | exit
                | throw
       .
lift_exn(F) when is_function(F, 1) ->
    fun(X) ->
        try
            {ok, F(X)}
        catch Class:Reason ->
            {error, {Class, Reason}}
        end
    end.

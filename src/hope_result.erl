-module(hope_result).

-behavior(hope_monad).

-export_type(
    [ t/2
    ]).

-export(
    [ return/1
    , map/2
    , pipe/2
    , lift_exn/1
    , lift_exn/2
    ]).


-type t(A, B) ::
      {ok, A}
    | {error, B}
    .


-spec return(A) ->
    {ok, A}.
return(X) ->
    {ok, X}.

-spec map(t(A, Error), fun((A) -> (B))) ->
    t(B, Error).
map({ok, X}, F) ->
    {ok, F(X)};
map({error, _}=Error, _) ->
    Error.

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

-spec lift_exn(F, Label) -> G
    when F     :: fun((A)-> B)
       , G     :: fun((A)-> t(B, {Label, {Class, Reason :: any()}}))
       , Class :: error
                | exit
                | throw
       .
lift_exn(F, Label) when is_function(F, 1) ->
    fun(X) ->
        try
            {ok, F(X)}
        catch Class:Reason ->
            {error, {Label, {Class, Reason}}}
        end
    end.

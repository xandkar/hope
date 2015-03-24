-module(hope_result).

-behavior(hope_gen_monad).

-export_type(
    [ t/2
    , exn_class/0
    , exn_value/1
    ]).

-export(
    [ return/1
    , map/2
    , map_error/2
    , tag_error/2
    , pipe/2
    , lift_exn/1
    , lift_exn/2
    , lift_map_exn/3
    ]).

-type exn_class() ::
      error
    | exit
    | throw
    .

-type exn_value(A) ::
    {exn_class(), A}.

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

-spec map_error(t(A, B), fun((B) -> (C))) ->
    t(A, C).
map_error({ok, _}=Ok, _) ->
    Ok;
map_error({error, Reason}, F) ->
    {error, F(Reason)}.

-spec tag_error(t(A, Reason), Tag) ->
    t(A, {Tag, Reason}).
tag_error({ok, _}=Ok, _) ->
    Ok;
tag_error({error, Reason}, Tag) ->
    {error, {Tag, Reason}}.

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
    when F     :: fun((A) -> B)
       , G     :: fun((A) -> t(B, exn_value(any())))
       .
lift_exn(F) when is_function(F, 1) ->
    ID = fun hope_fun:id/1,
    lift_map_exn(F, ID, ID).

-spec lift_exn(F, ErrorTag) -> G
    when F :: fun((A) -> B)
       , G :: fun((A) -> t(B, {ErrorTag, exn_value(any())}))
       .
lift_exn(F, ErrorTag) when is_function(F, 1) ->
    ID = fun hope_fun:id/1,
    Tag = fun (Reason) -> {ErrorTag, Reason} end,
    lift_map_exn(F, ID, Tag).

-spec lift_map_exn(F, MapOk, MapError) -> G
    when F        :: fun((A) -> B)
       , MapOk    :: fun((B) -> C)
       , MapError :: fun((exn_value(any())) -> Error)
       , G        :: fun((A) -> t(C, Error))
       .
lift_map_exn(F, MapOk, MapError) when is_function(F, 1) ->
    fun(X) ->
        Result =
            try
                {ok, F(X)}
            catch Class:Reason ->
                {error, {Class, Reason}}
            end,
        % Applying maps separately as to not unintentionally catch an exception
        % raised in a map.
        case Result
        of  {ok   , _}=Ok    -> map      (Ok   , MapOk)
        ;   {error, _}=Error -> map_error(Error, MapError)
        end
    end.

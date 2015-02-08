-module(hope_time).

-export_type(
    [ t/0
    ]).

-export(
    [ now/0
    , of_timestamp/1
    , to_unix_time/1
    , of_iso8601/1

    % Floatable
    , of_float/1
    , to_float/1

    % TODO: Stringable
    ]).


-define(T, #?MODULE).


-record(?MODULE,
    { unix_time :: float()
    }).

-opaque t() ::
    ?T{}.


-spec now() ->
    t().
now() ->
    Timestamp = os:timestamp(),
    of_timestamp(Timestamp).

-spec of_timestamp(erlang:timestamp()) ->
    t().
of_timestamp({MegasecondsInt, SecondsInt, MicrosecondsInt}) ->
    Million = 1000000.0,
    Megaseconds  = float(MegasecondsInt),
    Seconds      = float(SecondsInt),
    Microseconds = float(MicrosecondsInt),
    UnixTime = (Megaseconds * Million) + Seconds + (Microseconds / Million),
    ?T{unix_time = UnixTime}.

-spec to_unix_time(t()) ->
    float().
to_unix_time(?T{unix_time=UnixTime}) ->
    UnixTime.

-spec of_float(float()) ->
    t().
of_float(Float) when is_float(Float) ->
    ?T{unix_time = Float}.

-spec to_float(t()) ->
    float().
to_float(?T{unix_time=Float}) ->
    Float.

-spec of_iso8601(binary()) ->
    hope_result:t(t(), {unrecognized_as_iso8601, binary()}).
of_iso8601(<<Bin/binary>>) ->
    % We use regexp rather than just simple binary pattern match, because we
    % also want to validate character ranges, i.e., that components are
    % integers.
    ValidPatterns =
        [ {zoneless, <<"\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d">>}
        ],
    ValidPatternMatchers =
        [{Tag, make_regexp_bool(RegExp)} || {Tag, RegExp} <- ValidPatterns],
    case hope_list:first_match(ValidPatternMatchers, Bin)
    of  none            -> {error, {unrecognized_as_iso8601, Bin}}
    ;  {some, zoneless} -> {ok, of_iso8601_zoneless(Bin)}
    end.

-spec of_iso8601_zoneless(binary()) ->
    t().
of_iso8601_zoneless(<<Bin/binary>>) ->
    << YearBin:4/binary, "-", MonthBin:2/binary, "-", DayBin:2/binary
     , "T"
     , HourBin:2/binary, ":", MinBin:2/binary  , ":", SecBin:2/binary
    >> = Bin,
    Year  = binary_to_integer(YearBin),
    Month = binary_to_integer(MonthBin),
    Day   = binary_to_integer(DayBin),
    Hour  = binary_to_integer(HourBin),
    Min   = binary_to_integer(MinBin),
    Sec   = binary_to_integer(SecBin),
    DateTime = {{Year, Month, Day}, {Hour, Min, Sec}},
    SecondsGregorian = calendar:datetime_to_gregorian_seconds(DateTime),
    SecondsFromZeroToUnixEpoch = 62167219200,
    SecondsUnixEpochInt = SecondsGregorian - SecondsFromZeroToUnixEpoch,
    SecondsUnixEpoch = float(SecondsUnixEpochInt),
    of_float(SecondsUnixEpoch).

-spec make_regexp_bool(binary()) ->
    fun((binary()) -> boolean()).
make_regexp_bool(<<RegExp/binary>>) ->
    fun (<<String/binary>>) ->
        case re:run(String, RegExp)
        of  nomatch    -> false
        ;   {match, _} -> true
        end
    end.

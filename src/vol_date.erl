% @doc This module contains various date and time related functions.
%
% Right now, this module only contains addition and subraction of various types of time intervals.
-module(vol_date).

-export([ add/2
         ,add/3
        ,sub/2
        ,sub/3
        ,convert_to/2
        ,get_type/1
        ,truncate_to_hours/1
        ]).


% @type date() = {YYYY::non_neg_integer(),MM::non_neg_integer(),DD::non_neg_integer()}.
% @type time() = {HH::non_neg_integer(),MM::non_neg_integer(),SS::non_neg_integer()}.
% @type time_interval() = {datetime,{date(),time()}} |{date, date()}| {time,time()} | {(days|day),non_neg_integer()}  | {(hour|hours),non_neg_integer()} | {(second|seconds),non_neg_integer} |(Seconds :: non_neg_integer()).
% @type time_type() = datetime | time | date | day | days | hour | hours |second | seconds .
-type date() :: {pos_integer(),pos_integer(),pos_integer()}.
-type time() :: {pos_integer(),pos_integer(),pos_integer()}.
-type t(A,B) :: {A,B}.
-type t_date() :: t(date,date()).
-type t_time() :: t(time,time()).
-type t_datetime() :: t(datetime,{date(),time()}).
-type t_second() :: t(second|seconds,non_neg_integer()) | non_neg_integer().
-type t_hour() :: t(hour|hours,non_neg_integer()).
-type t_day() :: t(day|days,non_neg_integer()).
-type time_type() :: date | time | datetime | day | days | hour | hours | second | seconds .
-type time_interval() :: t_datetime() | t_date() | t_time() | t_second() | t_day() | t_hour().
    
-spec sub(time_interval(),time_interval()) -> time_interval().
% @doc Subtracts a time interval from another.
%
% Each interval is converted to seconds, and the first subtracted from the other. (Dates are intervals of time from some starting point to the date specified.)
% The result is then converted back to the type of the first argument and sent back.
%
% <code>
% > {@module}:sub({datetime,{{2017,7,4},{0,56,34}}},{hours,3}).<br/>
% {datetime,{{2017,7,3},{21,56,34}}}<br/>
% </code>

sub(A,B) ->
    sub(get_type(A),A,B).

-spec sub(time_type(),time_interval(),time_interval()) -> time_interval().
% @doc Subtracts a time interval from another, and converts the result.
%
% Each interval is converted to seconds, and the first subtracted from the other. (Dates are intervals of time from some starting point to the date specified.)
% The result is then converted to the type specified in the first argument.
%
% <code>
% > {@module}:sub(hours,{datetime,{{2017,7,4},{0,56,34}}},{hours,3}).<br/>
% {hours,17685093}
% </code>
sub(Type,A,B) ->
    seconds_to(Type,convert_to_seconds(A)-convert_to_seconds(B)).


-spec convert_to(time_type(),time_interval()) -> time_interval().
% @doc Converts to Type the time interval Time.
%
% <code>
% > {@module}:convert_to(time,{seconds,23020}).<br/>
% {time,{6,23,40}}<br/>
% </code>
% 
convert_to(Type,Time) ->
    seconds_to(Type,convert_to_seconds(Time)).

% @doc Adds a time interval to another.
% Each interval is converted to seconds, and then added together.
% The result is then converted back to the type of the first argument and sent back.
-spec add(time_interval(),time_interval()) -> time_interval().
add(A,B) ->
    seconds_to(get_type(A),convert_to_seconds(A)+convert_to_seconds(B)).

% @doc Adds a time interval to another, and converts the result.
% Each interval is converted to seconds, and then added together.
% The result is then converted back to the Cast type.
-spec add(time_type(),time_interval(),time_interval()) -> time_interval().
add(Cast,A,B) ->
    seconds_to(Cast,convert_to_seconds(A)+convert_to_seconds(B)).
    
-spec get_type(time_interval()) -> time_type().
% @doc Gets the type of a time expression
get_type({Type,_}) -> Type;
get_type(N) when is_integer(N) -> seconds.

convert_to_seconds({datetime,DT})        -> calendar:datetime_to_gregorian_seconds(DT);

convert_to_seconds({time,HMS})           -> calendar:time_to_seconds(HMS);

convert_to_seconds({days,D})             -> D*86400;
convert_to_seconds({day,D})             -> D*86400;

convert_to_seconds({hours,D})             -> D*3600;
convert_to_seconds({hour,D})             -> D*3600;

convert_to_seconds({date,YMD})           -> calendar:date_to_gregorian_days(YMD)*86400;

convert_to_seconds({seconds,S})          -> S;
convert_to_seconds({second,S})          -> S;

convert_to_seconds(A) when is_integer(A) -> A.
    
seconds_to(datetime,S) -> {datetime,calendar:gregorian_seconds_to_datetime(S)};

seconds_to(time,S) -> 
    {_,HMS}=calendar:gregorian_seconds_to_datetime(S),
    {time,HMS};

seconds_to(date,S) -> 
    {YMD,_}=calendar:gregorian_seconds_to_datetime(S),
    {date,YMD};

seconds_to(day,S) ->
    seconds_to(days,S);

seconds_to(days,S) ->
    {D,_HMS}=calendar:seconds_to_daystime(S),
    {days,D};

seconds_to(hours,S) -> {hours,trunc(S/3600)};
seconds_to(hour,S) -> {hours,trunc(S/3600)};

seconds_to(seconds,S) -> S;
seconds_to(second,S) -> S;

seconds_to(integer,S) -> S.

-spec truncate_to_hours(time_interval()) -> time_interval().
% @doc Truncates the time interval down to the nearest hour.
% Returns in the same interval format as the Time argument.
truncate_to_hours(Time) -> 
    Type = get_type(Time),
    Hours = convert_to(hours,Time),
    convert_to(Type,Hours).


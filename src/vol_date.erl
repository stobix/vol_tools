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


% @type date() = {YYYY::pos_integer(),MM::pos_integer(),DD::pos_integer()}.
% @type time() = {HH::pos_integer(),MM::pos_integer(),SS::pos_integer()}.
% @type time_interval() = {datetime,{date(),time()}} |{date, date()}| {time,time()} | {days,pos_integer()} | {day,pos_integer()} | {seconds,pos_integer()}|{second,pos_integer}.
% @type time_type() = datetime | time | date | day | days | second | seconds .
% @doc Subtracts a time interval from another.
%
% Each interval is converted to seconds, and the first subtracted from the other. (Dates are intervals of time from some starting point to the date specified.)
% The result is then converted back to the type of the first argument and sent back.
-type date() :: {pos_integer(),pos_integer(),pos_integer()}.
-type time() :: {pos_integer(),pos_integer(),pos_integer()}.
-type t(A,B) :: {A,B}.
-type t_date() :: t(date,date()).
-type t_time() :: t(time,time()).
-type t_datetime() :: t(datetime,{date(),time()}).
-type t_second() :: t(second|seconds,non_neg_integer()).
-type t_hour() :: t(hour|hours,non_neg_integer()).
-type t_day() :: t(day|days,non_neg_integer()).
-type time_type() :: date | time | datetime | second | seconds | day | days | hour | hours .
-type time_interval() :: t_datetime() | t_date() | t_time() | t_second() | t_day() | t_hour().
    
-spec sub(time_interval(),time_interval()) -> time_interval().

sub(A,B) ->
    sub(get_type(A),A,B).

-spec sub(time_type(),time_interval(),time_interval()) -> time_interval().
sub(Type,A,B) ->
    seconds_to(Type,convert_to_seconds(A)-convert_to_seconds(B)).


-spec convert_to(A::time_type(),time_interval()) -> time_interval().
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
    
get_type({Type,_}) -> Type;
get_type(N) when is_integer(N) -> integer.

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

seconds_to(integer,S) -> S.

-spec truncate_to_hours(time_interval()) -> time_interval().
truncate_to_hours(Time) -> 
    Type = get_type(Time),
    Hours = convert_to(hours,Time),
    convert_to(Type,Hours).


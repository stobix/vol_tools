-module(vol_date).

-export([ add/2
         ,add/3
        ,sub/2
        ]).


% General function for adding strange stuff together.
%add(T1={date,_},{date,YMD2}) ->
%    D2=calendar:date_to_gregorian_days(YMD2),
%    add(T1,D2);
%
%add({date,YMD1},D2) when is_integer(D2) ->
%    {date,calendar:gregorian_date_to_days(calendar:date_to_gregorian_days(YMD1)+D2)};
%
%add(D1,{date,YMD2}) when is_integer(D1) ->
%    {date,calendar:gregorian_date_to_days(D1+calendar:date_to_gregorian_days(YMD2))};
%
%add(T1={time,_},{time,HMS2}) ->
%    S2=calendar:time_to_seconds(HMS2),
%    add(T1,S2);
%
%add({time,HMS1},S2) when is_integer(S2) ->
%    {time,calendar:seconds_to_time(calendar:time_to_seconds(HMS1)+S2)};
%
%add(S1,{time,HMS2}) when is_integer(S1)->
%    {time,calendar:seconds_to_time(S1+calendar:time_to_seconds(HMS2))}.

sub(A,B) ->
    seconds_to(get_type(A),convert_to_seconds(A)-convert_to_seconds(B)).

add(A,B) ->
    seconds_to(get_type(A),convert_to_seconds(A)+convert_to_seconds(B)).

add(Cast,A,B) ->
    seconds_to(Cast,convert_to_seconds(A)+convert_to_seconds(B)).
    
get_type({Type,_}) -> Type;
get_type(N) when is_integer(N) -> integer.

convert_to_seconds({datetime,DT})        -> calendar:datetime_to_gregorian_seconds(DT);

convert_to_seconds({time,HMS})           -> calendar:time_to_seconds(HMS);

convert_to_seconds({days,D})             -> D*86400;
convert_to_seconds({day,D})             -> D*86400;

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

seconds_to(days,S) ->
    {D,_HMS}=calendar:seconds_to_daystime(S),
    {days,D};

seconds_to(seconds,S) -> S;

seconds_to(integer,S) -> S.


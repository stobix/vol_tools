-module(csvparser).

-export([
    get_all_data/2,
    get_all_data/3,
    get_next_data/1,
    get_next_data/2,
    get_next_data/3,
    result_to_csv/1,
    result_to_file/2,
    result_to_file/3
    ]).

% Internal callback exports
-export([
        convert_data_point_silent/1
    ]).

get_all_data(File,S) when is_list(File) ->
    {ok,FD}=file:open(File,[read]),
    get_all_data(FD,S);

get_all_data(FD,S) ->
    get_next_data(FD,S),% Dispose of the headers.
    get_all_data_(FD,S,[]).

get_all_data(FD,S1,S2) ->
    get_all_data(FD,{S1,S2}).

get_all_data_(FD,Sel,Acc) ->
    case get_next_data(FD,Sel) of
        eof -> lists:reverse(Acc);
        Data ->
            get_all_data_(FD,Sel,[Data|Acc])
    end.

get_next_data(FD,{Selection1,Selection2}) ->
    case get_next_data(FD) of
        ok -> eof;
        Data ->
            {pick_columns(Selection1,Data), pick_columns(Selection2,Data)}
    end;

get_next_data(FD,Selection) ->
    case get_next_data(FD) of
        ok -> eof;
        Data ->
            pick_columns(Selection,Data)
    end.


get_next_data(FD,Selection1,Selection2) ->
    case get_next_data(FD) of
        ok -> eof;
        Data ->
            {pick_columns(Selection1,Data), pick_columns(Selection2,Data)}
    end.

get_next_data(FD) ->
    case file:read_line(FD) of 
        {ok,Data} -> 
            Tokens=string:tokens(Data,";\n"),
            lists:map(fun ?MODULE:convert_data_point_silent/1,Tokens);
        {error,terminated} -> 
            file:close(FD);
        {error,badarg} ->
            ok;
        eof -> 
            file:close(FD) 
    end.

convert_data_point_silent(Point) ->
    case string:to_float(Point) of
        {error,no_float} ->
            case string:to_integer(Point) of
                {error,no_integer} ->
                    Point;
                {Integer,[]} ->
                    Integer;
                {_Integer,_Stuff} ->
                    Point
            end;
        {Float,[]} ->
            Float;
        {_Float,_Stuff} ->
            Point
    end.

pick_columns(ColumnsToPick,DataList) ->
    pick_columns(ColumnsToPick,DataList,0,length(DataList),[]).

pick_columns([],_,_,_,Acc) -> 
    lists:reverse(Acc);

pick_columns(_,_,J,J,Acc) ->
    lists:reverse(Acc);

pick_columns([C|ColumnsToPick],[D|DataList],C,J,Acc) ->
    pick_columns(ColumnsToPick,DataList,C+1,J,[D|Acc]);

pick_columns(ColumnsToPick,[_|DataList],C,J,Acc) ->
    pick_columns(ColumnsToPick,DataList,C+1,J,Acc).

-spec result_to_csv({list(number()),list(number())}) -> string().
result_to_csv(Result) ->
    string:join(
        lists:map(
            fun
                ({X,Y}) -> 
                    lists:flatten( 
                        string:join(
                            lists:map(
                                fun
                                    (Z) -> 
                                        io_lib:format("~w",[Z]) 
                                end,
                                lists:flatten([X,Y])),
                            ";")) 
            end,
            Result),
        "\n").

-spec result_to_file(FileName::string(),Result::{list(number()),list(number())},Prelude::string()) -> no_return().

result_to_file(FileName,Result,Prelude) ->
    file:write_file(FileName,[Prelude,result_to_csv(Result)]).

result_to_file(FileName,Result=[{A,B}|_]) ->
    Prelude=
            [string:join(lists:map(fun(X) -> "x"++integer_to_list(X) end, lists:seq(1,length(A))),";"),
             ";",
             string:join(lists:map(fun(Y) -> "y"++integer_to_list(Y) end, lists:seq(1,length(B))),";"),
            "\n"],

    result_to_file(FileName,Result,Prelude).


% @doc Functions for extracting columns from csv files into lists/tuples, and for writing csv files from similarly formatted lists/tuples of data..
%
% Currently supported data formats: <code>[_]</code> and <code>[{[_],[_]}]</code>
% @todo General format parsing.
-module(csvparser).

-export([
    get_all_data/2,
    %get_all_data/3,
    get_next_data/1,
    get_next_data/2,
    get_next_data/3,
    result_to_csv/1,
    result_to_file/2,
    result_to_file/3,
    lolon_to_file/2
    ]).

% Internal callback exports
-export([
        convert_data_point_silent/1
    ]).

-type csv_col_list_spec() :: [non_neg_integer()].
-type csv_col_tuple_spec() :: {[non_neg_integer()],[non_neg_integer()]}.
-type csv_col_spec() :: csv_col_list_spec() | csv_col_tuple_spec().

-type csv_col_tuple_data() :: {[non_neg_integer()],[non_neg_integer()]}.
-type csv_col_list_data() :: non_neg_integer().
-type csv_col_data() :: [csv_col_list_data()] | [csv_col_tuple_data()].

% @doc Extract all relevant csv data, excluding the first (header) line, from a file.
%
% The second parameter specifies which columns we want to include when getting the rows, and into what structure.
% get_all_data("file.csv",{[1],[0,3}) would return a list of tuples, where each tuple would have two elements; one element with the csv element from the first column wrapped in a list, and one element with the csv elements from the 0th and 2nd colmns wrapped in a list.
%
% Supported formats right now are lists and 2-tuples of lists.
%
% NOTE: Due to a bug in edoc, only half the type is shown in the specification above.
% The full spec is:
%
% <code>
%-spec get_all_data(file:filename(),csv_col_tuple_spec()) -> csv_col_tuple_data() 
%
%    ; (file:fd(),csv_col_list_spec()) -> csv_col_list_data().
% </code>
-spec get_all_data(file:filename(),csv_col_tuple_spec()) -> csv_col_tuple_data() 
    ; (file:filename(),csv_col_list_spec()) -> csv_col_list_data().

get_all_data(File,S) when is_list(File) ->
    {ok,FD}=file:open(File,[read]),
    get_all_data(FD,S);

get_all_data(FD,S) ->
    get_next_data(FD,S),% Dispose of the headers.
    get_all_data_(FD,S,[]).

% @private
% @doc Extract all remaining lines of data from a previously opened csv file.
%
% Alternate form for get_all_data/2. get_all_data(X,Y,Z) == get_all_data(X,{Y,Z}).

get_all_data_(FD,Sel,Acc) ->
    case get_next_data(FD,Sel) of
        eof -> lists:reverse(Acc);
        Data ->
            get_all_data_(FD,Sel,[Data|Acc])
    end.

% @doc Get the next line from the csv file.
%
% The second argument filters the columns as in @link{get_all_data/2}
%
% Returns the data point, or eof on EOF/error.
%
% NOTE: Due to a bug in edoc, only half the type is shown in the specification above.
% The full spec is:
%
% <code>
% -spec get_next_data(file:fd(),csv_col_tuple_spec()) -> csv_col_tuple_data() 
%     ; (file:fd(),csv_col_list_spec()) -> csv_col_list_data().
% </code>
-spec get_next_data(file:fd(),csv_col_tuple_spec()) -> csv_col_tuple_data() 
    ; (file:fd(),csv_col_list_spec()) -> csv_col_list_data().
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
% @doc Alternate form of get_next_data/2.
%
% get_next_data(X,Y,Z) == get_next_data(X,{Y,Z}).

get_next_data(FD,Selection1,Selection2) ->
    case get_next_data(FD) of
        ok -> eof;
        Data ->
            {pick_columns(Selection1,Data), pick_columns(Selection2,Data)}
    end.

% @doc Get the next line from the csv file.
%
% Integers and floats are converted, the rest are returned as-is.
-spec get_next_data(file:fd()) -> [integer()|float()|any()].
get_next_data(FD) ->
    case file:read_line(FD) of 
        {ok,Data} -> 
            Tokens=string:tokens(Data,";\n"),
            lists:map(fun convert_data_point_silent/1,Tokens);
        {error,terminated} -> 
            file:close(FD);
        {error,badarg} ->
            ok;
        eof -> 
            file:close(FD) 
    end.

% @private
% only exported since I use it in a mop
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


% @doc Converts a list of the same format that can be returned from get_all_data/2 to a csv string.
%
-spec result_to_csv(Result) -> string()
    when Result::list({list(number()),list(number())}|list()).
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
                            ";"));
                (X) when is_list(X) ->
                    lists:flatten( 
                        string:join(
                            lists:map(
                                fun
                                    (Z) -> 
                                        io_lib:format("~w",[Z]) 
                                end,
                                lists:flatten(X)),
                            ";"))
            end,
            Result),
        "\n").


% @doc Converts a list of the same format that can be returned from get_all_data/2 to a csv string, and writes it to a file.
%
% The Prelude is a string that will be put before the csv contents in the file.
%
-spec result_to_file(FileName::string(),Result::[{list(number()),list(number())}|list()],Prelude::string()) -> no_return().

result_to_file(FileName,Result,Prelude) ->
    file:write_file(FileName,[Prelude,result_to_csv(Result)]).
% @doc Takes a list of tuples of lists of values, makes a nice header, and writes it all to a csv file.
%
% Basically, this converts a list of [{InList,OutList}] into a csv file whose first line is x1;x2;x3...;y1,y2... where the number of x:es and y:s are the same as the lengths of InList and OutList, respectively.
% The rest of the lines are the actual values for the parameters.
%
% Note that this version of result_to_file has a more strict requirement on the result type than has result_to_file/3.
result_to_file(FileName,Result=[{A,B}|_]) ->
    Prelude=
            [string:join(lists:map(fun(X) -> "x"++integer_to_list(X) end, lists:seq(1,length(A))),";"),
             ";",
             string:join(lists:map(fun(Y) -> "y"++integer_to_list(Y) end, lists:seq(1,length(B))),";"),
            "\n"],

    result_to_file(FileName,Result,Prelude).

% @doc Converts a list of lists of numbers to a csv string, and writes it to a file.
%
% Anything that is not numbers is passed through as-is.

lolon_to_file(FileName,LoLoN) when is_list(LoLoN) andalso is_list(hd(LoLoN)) -> % andalso is_number(hd(hd(LoLoN))) ->
    NumToList= fun
        (N) when is_float(N) -> float_to_list(N); 
        (N) when is_integer(N) -> integer_to_list(N); 
        (X) -> X 
    end,
    ListOfNumToListOfList = fun (LoN) -> lists:map(NumToList,LoN) end,
    ListNumbers = lists:map(ListOfNumToListOfList,LoLoN),
    ListToLine=fun(X) -> string:join(X,";") end,
    Lines=lists:map(ListToLine,ListNumbers),
    File=string:join(Lines,"\n"),
    file:write_file(FileName,File).


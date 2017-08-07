% @doc Folds and maps over deeply nested structures.
%
% Defines "maps" and "folds" on any recursive list/tuple combination, calling a function on anything it finds that is not a tuple or a list.
-module(vol_struct).


-export([
        deep_map/2,
        deep_map_num/2,
        deep_foldl1/2,
        deep_foldl/3,
        deep_foldl1_num/2,
        deep_foldl_num/3
    ]).

% @doc Deep map over all non-structure elements of a structure.
%
% Applies Fun to anything that is not a tuple or a list.
%
% Recurses into all tuples and lists.
%
% Returns the same structure as it is given.
deep_map(Fun,S) when is_tuple(S) ->
    list_to_tuple(deep_map(Fun,tuple_to_list(S)));

deep_map(Fun,[A|B]) ->
    [deep_map(Fun,A)|deep_map(Fun,B)];

deep_map(_Fun,[]) -> [];

deep_map(Fun,A) -> Fun(A).


% @doc Deep map over all numbers inside a structure.
%
% Recurses into all tuples and lists.
%
% Returns the same structure as it is given.
deep_map_num(Fun,S) when is_tuple(S)->
    list_to_tuple(deep_map_num(Fun,tuple_to_list(S)));

deep_map_num(Fun,[A|B]) ->
    [deep_map_num(Fun,A)|deep_map_num(Fun,B)];

deep_map_num(Fun,A) when is_number(A) -> Fun(A);

deep_map_num(_Fun,A) -> A.


% @doc Deep fold over all non-structure elements of a structure.
%
% Applies Fun to anything that is not a tuple or a list.
%
% Recurses into all tuples and lists, beginning with the rightmost element, depth first.
%
% Example: deep_foldl(fun(X,Y) -> [X|Y] end,[],[[{a},[[b]]],c,{[d,e]}] == [a,b,c,d,e]
-spec deep_foldl(fun( (any(),A) -> any() ), A,Structure::any()) -> any().
deep_foldl(Fun,Acc,S) when is_tuple(S) ->
    deep_foldl(Fun,Acc,tuple_to_list(S));

deep_foldl(Fun,Acc,[A|B]) ->
    Bcc = deep_foldl(Fun,Acc,A),
    deep_foldl(Fun,Bcc,B);

deep_foldl(_Fun,Acc,[]) -> Acc;

deep_foldl(Fun,Acc,A) -> Fun(A,Acc).
    

% @doc Deep fold over all non-structure elements of a structure, using the first element as initial acc.
%
% Applies Fun to anything that is not a tuple or a list.
%
% Recurses into all tuples and lists, beginning with the rightmost element, depth first.
%
-spec deep_foldl1(fun( (any(),any()) -> any() ), StructureContainingAtLeastTwoElements::any()) -> any().
deep_foldl1(Fun,Thing) ->
    {element,FirstElem}=find_first_element(Thing),
    deep_foldl(Fun,FirstElem,Thing).


find_first_element(A) when is_tuple(A) ->
    find_first_element(tuple_to_list(A));

find_first_element([A|B]) -> 
    case find_first_element(A) of
        false ->find_first_element(B);
        Element -> Element
    end;

find_first_element({}) ->
    false;

find_first_element([]) ->
    false;

find_first_element(A) -> {element,A}.

% @doc Deep fold over all numeric elements of a structure.
%
% Applies Fun to anything that is not a tuple or a list.
%
% Recurses into all tuples and lists, beginning with the rightmost element, depth first.
%
% Example: <fix><code>
%        Structure=[[{1},[1]],2,{[1,a]}],
%        Add = fun(X,Y) -> X+Y end,
%        deep_foldl(Add,0,Structure) -> 5
% </code></fix>
%
deep_foldl_num(Fun,Acc,S) when is_tuple(S) ->
    deep_foldl_num(Fun,Acc,tuple_to_list(S));

deep_foldl_num(Fun,Acc,[A|B]) ->
    Bcc = deep_foldl_num(Fun,Acc,A),
    deep_foldl_num(Fun,Bcc,B);

deep_foldl_num(Fun,Acc,A) when is_number(A) -> Fun(A,Acc);

deep_foldl_num(_Fun,Acc,_A) -> Acc.

% @doc Deep fold over all numeric elements of a structure, using first element as initial return value.
%
% Applies Fun to anything that is not a tuple or a list.
%
% Recurses into all tuples and lists, beginning with the rightmost element, depth first.
%
% Example: <code>
%        Structure=[[{1},[1]],2,{[1,a]}],
%        Add = fun(X,Y) -> X+Y end,
%        deep_foldl1(Add,Structure) -> 5
% </code>
deep_foldl1_num(Fun,Thing) ->
    {element,FirstElem}=find_first_element_num(Thing),
    deep_foldl_num(Fun,FirstElem,Thing).


find_first_element_num(A) when is_tuple(A) ->
    find_first_element_num(tuple_to_list(A));

find_first_element_num([A|B]) -> 
    case find_first_element_num(A) of
        false ->find_first_element_num(B);
        Element -> Element
    end;

find_first_element_num({}) ->
    false;

find_first_element_num([]) ->
    false;

find_first_element_num(A) when is_number(A) -> {element,A};

find_first_element_num(_A) -> false.

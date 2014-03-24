-module(vol_struct).

-export([
        deep_map/2,
        deep_map_num/2,
        deep_foldl1/2,
        deep_foldl/3,
        deep_foldl1_num/2,
        deep_foldl_num/3
    ]).

deep_map(Fun,S) when is_tuple(S) ->
    list_to_tuple(deep_map(Fun,tuple_to_list(S)));

deep_map(Fun,[A|B]) ->
    [deep_map(Fun,A)|deep_map(Fun,B)];

deep_map(Fun,[]) -> [];

deep_map(Fun,A) -> Fun(A).



deep_map_num(Fun,S) when is_tuple(S)->
    list_to_tuple(deep_map_num(Fun,tuple_to_list(S)));

deep_map_num(Fun,[A|B]) ->
    [deep_map_num(Fun,A)|deep_map_num(Fun,B)];

deep_map_num(Fun,A) when is_number(A) -> Fun(A);

deep_map_num(Fun,A) -> A.


deep_foldl(Fun,Acc,S) when is_tuple(S) ->
    deep_foldl(Fun,Acc,tuple_to_list(S));

deep_foldl(Fun,Acc,[A|B]) ->
    Bcc = deep_foldl(Fun,Acc,A),
    deep_foldl(Fun,Bcc,B);

deep_foldl(Fun,Acc,[]) -> Acc;

deep_foldl(Fun,Acc,A) -> Fun(A,Acc).
    

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

deep_foldl_num(Fun,Acc,S) when is_tuple(S) ->
    deep_foldl_num(Fun,Acc,tuple_to_list(S));

deep_foldl_num(Fun,Acc,[A|B]) ->
    Bcc = deep_foldl_num(Fun,Acc,A),
    deep_foldl_num(Fun,Bcc,B);

deep_foldl_num(Fun,Acc,A) when is_number(A) -> Fun(A,Acc);

deep_foldl_num(Fun,Acc,A) -> Acc.

deep_foldl1_num(Fun,Thing) ->
    {element,FirstElem}=find_first_element(Thing),
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

find_first_element_num(A) -> false.

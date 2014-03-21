-module(lst_ext).

-export([
    sift/2,
    siftmap/2,
    mapsiftr/3,
    mapsiftmapr/3,
    mapsiftl/3,
    mapsiftmapl/3,
    select_one/1,
    pick_nth/2,
    pick_nth_nonrev/2,
    pick_random/1,
    reorder/1,
    pick_n/2,
    pick_n_undef/2,
    pick_n_random/2,
    sequences/2,
    split_at/2,
    sequence_split/3
    ]).

-spec sift(function(),list(Thing::any())) -> {list(Thing),list(Thing)}.
sift(Pred,List) ->
    sift(Pred,List,[],[]).

sift(_,[],Acc,Bcc) ->
    {lists:reverse(Acc),lists:reverse(Bcc)};

sift(Pred,[L|List],Acc,Bcc) ->
    case Pred(L) of
        true ->
            sift(Pred,List,[L|Acc],Bcc);
        false ->
            sift(Pred,List,Acc,[L|Bcc])
    end.


-spec siftmap(function(),list(Thing::any())) -> {list(Thing),list(Thing)}.
siftmap(Pred,List) ->
    siftmap(Pred,List,[],[]).

siftmap(_,[],Acc,Bcc) ->
    {lists:reverse(Acc),lists:reverse(Bcc)};

siftmap(Pred,[L|List],Acc,Bcc) ->
    case Pred(L) of
        {true,Val} ->
            siftmap(Pred,List,[Val|Acc],Bcc);
        false ->
            siftmap(Pred,List,Acc,[L|Bcc])
    end.

-spec mapsiftmapl(Map::any(),function(),list(Thing::any())) -> {list(Thing),list(Thing)}.
mapsiftmapl(Items,Pred,List) ->
    lists:mapfoldl(
        fun(Item,SubList) ->
                {Res,Rest} = siftmap(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

-spec mapsiftl(Map::any(),function(),list(Thing::any())) -> {list(Thing),list(Thing)}.
mapsiftl(Items,Pred,List) ->
    lists:mapfoldl(
        fun(Item,SubList) ->
                {Res,Rest} = sift(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

-spec mapsiftmapr(Map::any(),function(),list(Thing::any())) -> {list(Thing),list(Thing)}.
mapsiftmapr(Items,Pred,List) ->
    lists:mapfoldr(
        fun(Item,SubList) ->
                {Res,Rest} = siftmap(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

-spec mapsiftr(Map::any(),function(),list(Thing::any())) -> {list(Thing),list(Thing)}.
mapsiftr(Items,Pred,List) ->
    lists:mapfoldr(
        fun(Item,SubList) ->
                {Res,Rest} = sift(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

% @doc transforms [1,2,3,...] to [[1,2,3..],[2,3,4...],...] where each sub list is of length Length, and the total list is of length length(List)-Length. Good for sliding window operations and such.
sequences(Length,List) when Length > length(List) ->
    [];

sequences(Length,List) ->
    [pick_n(Length,List)|sequences(Length,tl(List))].

% @doc runs sequence on the list, and runs a split_at on each sublist. E.g sequence_split(SeuqenceLength=3,SplitAt=2,List=lists:seq(1,100)) -> [{[1,2],[3]},{[2,3],[4]}....]
sequence_split(SequenceLength,SplitAt,List) ->
    Split = fun(SubList) -> 
            split_at(SplitAt,SubList) 
    end,
    lists:map(Split,sequences(SequenceLength,List)).

% @doc splits a list at a certain point.
% @spec (N::non_neg_integer(),list()) -> {L::list(),M::list()}, length(L) ==N.
split_at(N,List) ->
    {pick_n(N,List),lists:nthtail(N,List)}.



select_one(List) ->
    lists:nth(random:uniform(length(List)),List).

pick_nth(_,N) when N =< 0 -> false;
pick_nth(L,N) when N > length(L) -> false;

pick_nth(List,N) ->
    pick_nth(List,N,[]).

pick_nth([L|Ls],1,Before) -> {L,(lists:reverse(Before))++Ls};
pick_nth([L|Ls],N,Before) -> pick_nth(Ls,N-1,[L|Before]).


pick_nth_nonrev(_,N) when N =< 0 -> false;
pick_nth_nonrev(L,N) when N > length(L) -> false;

pick_nth_nonrev(List,N) ->
    pick_nth_nonrev(List,N,[]).

pick_nth_nonrev([L|Ls],1,Before) -> {L,Before++Ls};
pick_nth_nonrev([L|Ls],N,Before) -> pick_nth_nonrev(Ls,N-1,[L|Before]).

pick_random(List) ->
    pick_nth_nonrev(List,random:uniform(length(List))).

pick_n_random(N,List) ->
    pick_n_random(N,List,[]).


pick_n_random(0,List,Acc) -> {Acc,List};
pick_n_random(_N,[],Acc) -> {Acc,[]};
pick_n_random(N,List,Acc) ->
    {Item,NewList}=pick_random(List),
    pick_n_random(N-1,NewList,[Item|Acc]).
    

reorder(List) ->
    lists:foldr(
        fun
            (_,{[A],Acc}) ->
                [A|Acc];
            (_,{As,Acc}) ->
                {Nth,List1}=pick_nth(As,random:uniform(length(As))),
                {List1,[Nth|Acc]};
            (_,As) ->
                {Nth,List1}=pick_nth(As,random:uniform(length(As))),
                {List1,[Nth]}
        end,
        List,
        List).

pick_n(A,B) -> pick_n(A,B,[]).

pick_n(0,_,Acc) -> lists:reverse(Acc);
pick_n(N,[L|Ls],Acc) -> pick_n(N-1,Ls,[L|Acc]).

pick_n_undef(A,B) -> pick_n_undef(A,B,[]).
pick_n_undef(0,_,Acc) -> lists:reverse(Acc);
pick_n_undef(N,[L|Ls],Acc) -> pick_n_undef(N-1,Ls,[L|Acc]);
pick_n_undef(N,[],Acc) -> pick_n_undef(N-1,[],[undef|Acc]).


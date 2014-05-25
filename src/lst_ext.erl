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
    select_keyvals/3,
    select_vals/4,
    reorder/1,
    pick_n/2,
    pick_n_undef/2,
    pick_n_random/2,
    sequences/2,
    split_at/2,
    sequence_split/3,
    uminsert/2,
    uinsert/2,
    ruminsert/2,
    ruinsert/2,
    is_in/2,
    keyumerge/3,
    deep_map/2,
    uniq/1
    ]).

deep_map(Fun,[A|B]) ->
    [deep_map(Fun,A)|deep_map(Fun,B)];

deep_map(Fun,[]) -> [];

deep_map(Fun,A) -> Fun(A).

% TODO: Design and implement funtions that does ([a],[b,a,c]) -> [b,c,a]/[a,b,c] instead of ([a],[b,a,c]) -> [b,a,c].


-spec keyumerge(N::pos_integer(),[A::tuple(any())],[B::tuple(any())]) %when N =< size(A) andalso size(A)==size(B) 
    -> [tuple(any())].

keyumerge(N,A,B) ->
        keyumerge(N,A,B,[]).

keyumerge(N,[],[],Acc) ->
        Acc;

keyumerge(N,L1,[],Acc) ->
        L1++Acc;

keyumerge(N,[],L2,Acc) ->
        L2++Acc;

keyumerge(N,LL1=[I1|L1],LL2=[I2|L2],Acc) ->
        A=element(N,I1),
        B=element(N,I2),
        if A == B ->
                        keyumerge(N,L1,L2,[I1|Acc]);
                A < B ->
                        keyumerge(N,L1,LL2,[I1|Acc]);
                A > B ->
                        keyumerge(N,LL1,L2,[I2|Acc])
                end.
    

uniq(L) -> uniq(lists:sort(L),[]).

uniq([],Bs) -> lists:reverse(Bs);
uniq([A|As],[B|Bs]) when A == B -> uniq(As,[B|Bs]);
uniq([A|As],Bs) -> uniq(As,[A|Bs]).


-spec uminsert([A::any()],[A::any()]) -> [A::any()].
uminsert(As,Bs) ->
    lists:foldr(fun uinsert/2,Bs,As).

-spec uinsert(A::any(),[A::any()]) -> [A::any()].
uinsert(A,Bs) ->
    case is_in(A,Bs) of
        true -> Bs;
        false -> [A|Bs]
    end.

-spec is_in(A::any(),[A::any()]) -> boolean().

is_in(A,[]) -> false;

is_in(A,[B|Bs]) ->
    if A==B -> true;
        true -> is_in(A,Bs)
    end.

ruinsert(A,Bs) ->
    lists:reverse(uinsert(A,lists:reverse(Bs))).

ruminsert(As,Bs) ->
    lists:reverse(uminsert(lists:reverse(As),lists:reverse(Bs))).



-spec select_keyvals(list(any()),pos_integer(),list(any())) -> list(any()).
select_keyvals(Keys,KeyPos,KeyVals) ->
    lists:filter(fun(X) -> lists:member(element(KeyPos,X),Keys) end,KeyVals).

-spec select_vals(list(any()),pos_integer(),pos_integer(),list(any())) -> list(any()).
select_vals(Keys,KeyPos,ValPos,KeyVals) ->
    {RetVals,_}=lists:foldr(
        fun(K,{Acc,KV}) -> 
                case lists:keytake(K,KeyPos,KV) of
                    {value,KeyVal,OthVals} ->
                        {[element(ValPos,KeyVal)|Acc],OthVals};
                    false ->
                        {[{not_found,K}|Acc],KV}
                end
        end,
        {[],KeyVals},
        Keys),
    RetVals.



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


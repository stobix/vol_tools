-module(lst_ext).

-export([
    sift/2,
    siftmap/2,
    mapsiftr/3,
    mapsiftmapr/3,
    mapsiftl/3,
    mapsiftmapl/3
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

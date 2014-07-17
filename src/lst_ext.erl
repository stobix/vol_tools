% @doc Extends the lists module with various list related functions.
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
    pick_n/3,
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

% @doc Maps a function to all elements of a deep list.
% E.g: deep_map(fun(X) -> X + 1 end, [[1,[2]],3]) ->  [[2,[3]],4].
-type deep_list(A) ::[ A | deep_list(A)].
-spec deep_map(fun((A) -> B),List::deep_list(A)) -> B.

deep_map(Fun,[A|B]) ->
[deep_map(Fun,A)|deep_map(Fun,B)];

deep_map(_Fun,[]) -> [];

deep_map(Fun,A) -> Fun(A).

% TODO: Design and implement funtions that does ([a],[b,a,c]) -> [b,c,a]/[a,b,c] instead of ([a],[b,a,c]) -> [b,a,c].


-spec keyumerge(N::pos_integer(),[A::tuple(any())],[B::tuple(any())]) %when N =< size(A) andalso size(A)==size(B) 
    -> [tuple(any())].

    % @doc Makes a distinct, sorted merge between two sorted tuple lists.
    % If two elements have the same key, the element from the first list is used.
    % This is basically lists:umerge for keyval lists.
    keyumerge(N,A,B) ->
    keyumerge(N,A,B,[]).

    keyumerge(_N,[],[],Acc) ->
    Acc;

    keyumerge(_N,L1,[],Acc) ->
    L1++Acc;

    keyumerge(_N,[],L2,Acc) ->
    L2++Acc;

    keyumerge(N,LL1=[I1|L1],LL2=[I2|L2],Acc) ->
    A=element(N,I1),
    B=element(N,I2),
    if 
    A == B ->
    keyumerge(N,L1,L2,[I1|Acc]);
    A < B ->
    keyumerge(N,L1,LL2,[I1|Acc]);
    A > B ->
keyumerge(N,LL1,L2,[I2|Acc])
    end.


    % @doc Only keeps one of each element.
    % Comparison by '=='/2.
    uniq(L) -> uniq(lists:sort(L),[]).

    uniq([],Bs) -> lists:reverse(Bs);
    uniq([A|As],[B|Bs]) when A == B -> uniq(As,[B|Bs]);
    uniq([A|As],Bs) -> uniq(As,[A|Bs]).


% @doc unique map insert
%
% Insert all of the elements from the first list into the second that are not already there.
-spec uminsert([A],[A]) -> [A].
uminsert(As,Bs) ->
    lists:foldr(fun uinsert/2,Bs,As).


% @doc unique insert
%
% Insert an element into a list, unless it is already there.

-spec uinsert(A,List::[A]) -> [A].

uinsert(A,Bs) ->
    case is_in(A,Bs) of
        true -> Bs;
        false -> [A|Bs]
    end.

-spec is_in(A,List::[A]) -> boolean().

% @doc Basically {@link lists:member/2} with a nicer name.
is_in(_A,[]) -> false;

is_in(A,[B|Bs]) ->
    if A==B -> true;
        true -> is_in(A,Bs)
    end.
% @doc Reverse unique insert
%
%Inserts an element to the end of a list, unless it is already in the list.
-spec ruinsert(A,List::[A]) -> [A].

ruinsert(A,Bs) ->
    lists:reverse(uinsert(A,lists:reverse(Bs))).

-spec ruminsert(List1::[A],List2::[A]) -> [A].

% @doc Reverse unique map insert
%
% For each of the elements from the first list ,insert it to the end of the second list, unless it is already there.
ruminsert(As,Bs) ->
    lists:reverse(uminsert(lists:reverse(As),lists:reverse(Bs))).



% @doc Picks all tuples that match any of the keys in the keylist.
%
% Takes out all tuples from KeyVals that has a key in Keys at tuple position KeyPos.
%
% Example:
% select_keyvals([a],2,[{a,x,z},{b,a},{c,a,w}]) -> [{b,a},{c,a,w}].

-spec select_keyvals([any()],pos_integer(),[tuple(X)]) -> [tuple(X)].
select_keyvals(Keys,KeyPos,KeyVals) ->
    lists:filter(fun(X) -> lists:member(element(KeyPos,X),Keys) end,KeyVals).

% @doc Picks all values that matches the keys in the keylist.
%
% Takes out the value at position ValPos from all tuples from KeyVals whose key at position KeyPos is in Keys.
% 
% Crashes if the value position is outside the tuple boundaries.
%
% Example:
% select_vals([a],2,1,[{a,x,z},{b,a},{c,a,w}]) -> [b,c].
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

-type sorting_fun(X) :: fun( (X) -> boolean () ).
-type sortmap_fun(X,Y) :: fun( (X) -> {true,Y} | false).
-type constructor(X,Y) :: fun( (X) -> Y ).

% @doc Splits the list into values for which the function returns true, and values for which the function returns false.
%
% Example: <code>sift(fun(X) -> X == 3 end,[1,3,4]) -> {[3],[1,4]}.</code>
-spec sift(sorting_fun(X),List::[X]) -> {Matching::[X],NotMatching::[X]}.

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


% @doc Changes the values that matches, returns both matching and non-matching values in a tuple.
%
% Basically a map that sifts out values that are not to be mapped.
%
% Example: sift(fun(X) -> if X == 3 -> {true,a}; true -> false end,[1,3,4]) -> {[a],[1,4]}.
-spec siftmap(sortmap_fun(X,Y),List::[X]) -> {Matching::[Y],NotMatching::[X]}.

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


% @doc The same as mapsiftl, but with the option to change the matching elements.
%
-spec mapsiftmapl([X],constructor(X,sortmap_fun(Y,Z)),[Y]) -> {[{X,[Z]}],[Y]}.
mapsiftmapl(Items,Pred,List) ->
    lists:mapfoldl(
        fun(Item,SubList) ->
                {Res,Rest} = siftmap(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

% @doc Pairs elements with their matching values, according to a match function. Returns a tuple of the matches and unmatched values.
%
% Essentially, combines a sift with a lists:mapfoldl/3.
%
% Takes in a list of values (A) to be mapped over, a constructor (B) that converts the values of the list A into sift functions and a list (C) that is to be partitioned up according to the sift functions.
%
% Each element X in A is sent to B to construct a sift function that tells whether elements in C belongs to X or not.
%
% Each sift function is run in turn (in a fold), and the corresponding elements Y from C are picked and paired up with the corresponding element X from A before sending the rest of C to the next sift function.
%
% Note that each element from C only end up in the first pairing where they match.
%
% 
% Returns a tuple of all pairings {(X),[(Y)]} that could be made, together with any elements that didn't match any of the sift functions.

% Maps the constructor over every element in the first list. Sifts the sec
% Takes in a list Map of elements. Each element is, in order, passed to MapFun to produce the SiftFun for the element. The element runs
% 
-spec mapsiftl(A::[X],B::constructor(X,sorting_fun(Y)),C::[Y]) -> {[{X,[Y]}],[Y]}.
mapsiftl(Items,Pred,List) ->
    lists:mapfoldl(
        fun(Item,SubList) ->
                {Res,Rest} = sift(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

% @doc The right fold version of {@link mapsiftmapl/3}.
-spec mapsiftmapr([X],constructor(X,sortmap_fun(Y,Z)),[Y]) -> {[{X,[Z]}],[Y]}.
mapsiftmapr(Items,Pred,List) ->
    lists:mapfoldr(
        fun(Item,SubList) ->
                {Res,Rest} = siftmap(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

% @doc The right fold version of {@link mapsiftl/3}.
-spec mapsiftr(A::[X],B::constructor(X,sorting_fun(Y)),C::[Y]) -> {[{X,[Y]}],[Y]}.
mapsiftr(Items,Pred,List) ->
    lists:mapfoldr(
        fun(Item,SubList) ->
                {Res,Rest} = sift(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

% @doc Splits a list into smaller sequences
%
% Makes a list of the N first elements of L, then the N first elements of tl(L) and so on until the last N elements of L.
%
% E.g. transforms [1,2,3,...] to [[1,2,3..],[2,3,4...],...] where each sub list is of length Length, and the total list is of length length(List)-Length. Good for sliding window operations and such.
-spec sequences(pos_integer(),List::[X]) -> [[X]].
sequences(Length,List) when Length > length(List) ->
    [];

sequences(Length,List) ->
    [pick_n(Length,List)|sequences(Length,tl(List))].

% @doc Runs {@link sequence/2} on the list, and runs {@link split_at/2} on each resulting sublist. 
%
% E.g sequence_split(3,2,List=lists:seq(1,100)) -> [{[1,2],[3]},{[2,3],[4]}....]
-spec sequence_split(pos_integer(),pos_integer(),List::[X]) -> [{[X],[X]}].
sequence_split(SequenceLength,SplitAt,List) ->
    Split = fun(SubList) -> 
            split_at(SplitAt,SubList) 
    end,
    lists:map(Split,sequences(SequenceLength,List)).

% @doc splits a list at a certain point.
% @end
% , length(L) ==N.
-spec split_at(non_neg_integer(),List::[X]) -> {[X],[X]}
    when X :: any().
split_at(N,List) ->
    {pick_n(N,List),lists:nthtail(N,List)}.


% @doc Picks an element at random, and returns it discarding the rest of the list.
-spec select_one(List::[X]) -> X.
select_one(List) ->
    lists:nth(random:uniform(length(List)),List).

% @doc Picks the nth element of a list, and returns it together with the rest of the list.
% Returns false if no such element exists.
-spec pick_nth([X],pos_integer()) -> {X,[X]}.

pick_nth(_,N) when N =< 0 -> false;
pick_nth(L,N) when N > length(L) -> false;

pick_nth(List,N) ->
    pick_nth(List,N,[]).

pick_nth([L|Ls],1,Before) -> {L,(lists:reverse(Before))++Ls};
pick_nth([L|Ls],N,Before) -> pick_nth(Ls,N-1,[L|Before]).


% @doc The same as pick_nth, but does not reverse its arguments on return.
% For efficiency reasons.
-spec pick_nth_nonrev([X],pos_integer()) -> {X,[X]}.
pick_nth_nonrev(_,N) when N =< 0 -> false;
pick_nth_nonrev(L,N) when N > length(L) -> false;

pick_nth_nonrev(List,N) ->
    pick_nth_nonrev(List,N,[]).

pick_nth_nonrev([L|Ls],1,Before) -> {L,Before++Ls};
pick_nth_nonrev([L|Ls],N,Before) -> pick_nth_nonrev(Ls,N-1,[L|Before]).

% @doc Pick an element at random from the list, and return the element together with the rest of the list.
%
% The order of the list is not guaranteed to be maintained.

-spec pick_random([X]) -> {X,[X]}.
pick_random(List) ->
    pick_nth_nonrev(List,random:uniform(length(List))).

% @doc pick N elements at random from the list, and return those elements together with the rest of the list.
-spec pick_n_random(non_neg_integer(),[X]) -> {[X],[X]}
    when X::any().
pick_n_random(N,List) ->
    pick_n_random(N,List,[]).

pick_n_random(0,List,Acc) -> {Acc,List};
pick_n_random(_N,[],Acc) -> {Acc,[]};
pick_n_random(N,List,Acc) ->
    {Item,NewList}=pick_random(List),
    pick_n_random(N-1,NewList,[Item|Acc]).
    
% @doc Randomly reorder a list.
-spec reorder([X]) -> [X]
    when X :: any().
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

% @doc picks the first N elements from a list. 
%
% Crashes if N is bigger than the length of the list.
%
% pick_n(3,[1,2,3,4]) -> [1,2,3].
%
% pick_n(3,[1,2]) throws an exception.
-spec pick_n(non_neg_integer(),[X]) -> [X] when X :: any().
pick_n(A,B) -> pick_n_(A,B,[]).

pick_n_(0,_,Acc) -> lists:reverse(Acc);
pick_n_(N,[L|Ls],Acc) -> pick_n_(N-1,Ls,[L|Acc]).

% @doc Non-crashing version of {@link pick_n/2}.
%
% Returns undef for any element that would have been inserted had the list been long enough.
% pick_n(3,[1,2,3,4]) -> [1,2,3].
%
% pick_n(3,[1,2]) -> [1,2,undef].

pick_n_undef(A,B) -> pick_n_undef(A,B,[]).
pick_n_undef(0,_,Acc) -> lists:reverse(Acc);
pick_n_undef(N,[L|Ls],Acc) -> pick_n_undef(N-1,Ls,[L|Acc]);
pick_n_undef(N,[],Acc) -> pick_n_undef(N-1,[],[undef|Acc]).

% @doc Non-crashing version of {@link pick_n/2}, with a default value.
%
% Returns the default value for any element that would have been inserted had the list been long enough.
% pick_n(3,[1,2,3,4]) -> [1,2,3].
%
% pick_n(3,[1,2]) -> [1,2,undef].
pick_n(A,B,Y) -> pick_n_(A,B,Y,[]).

pick_n_(0,_,_Y,Acc) -> lists:reverse(Acc);
pick_n_(N,[L|Ls],_Y,Acc) -> pick_n_(N-1,Ls,[L|Acc]);
pick_n_(N,[],Y,Acc) -> pick_n_(N-1,[],[Y|Acc]).

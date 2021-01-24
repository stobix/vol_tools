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
    pick_elems/2,
    pick_n_undef/2,
    pick_n_random/2,
    sequences/2,
    split_at/2,
    sequence_split/3,
    uinsert/2,
    uminsert/2,
    ruminsert/2,
    ruinsert/2,
    is_in/2,
    keyumerge/3,
    keyudmerge/3,
    unsort_keymerge/3,
    deep_map/2,
    deep_foldl/3,
    deep_mapfoldl/3,
    uniq/1,
    keyuniq/2,
    skeyuniq/2,
    lunzip/1,
    unzip/1,
    create_empty_lol/1,
    create_empty_tol/1,
    assign_tuple/2,
    group/1,
    group_by_nth/2,
    set_equiv/2
    ]).

-spec unzip(ListOfTuples::[tuple()]) -> TupleOfLists::tuple().
% @doc unzips a list of ntuples with the same size N into an ntuple with N lists
%
% Calls lists:unzip/1, lists:unzip3/1 or lunzip/1 as needed.
unzip(LoT) when size(hd(LoT)) == 2 -> lists:unzip(LoT);
unzip(LoT) when size(hd(LoT)) == 3 -> lists:unzip3(LoT);
unzip(LoT) -> list_to_tuple(lunzip(LoT)).

-spec lunzip(ListOfLists::[[]]) -> TupleOfLists::tuple().
% @doc unzips a list of lists of size N into an list of N lists
lunzip(LoL) ->
    Size=length(hd(LoL)),
    LoL1=create_empty_lol(Size),
    lists:map(fun lists:reverse/1,lists:foldl(fun assign_tuple/2 ,LoL1,LoL)).

% @private
% @doc
% creates an empty tuple of lists structure
create_empty_tol(N) -> list_to_tuple(create_empty_lol(N)).

% @private
% creates an empty list of lists structure
create_empty_lol(0) ->[];
create_empty_lol(N) ->
    [[]|create_empty_lol(N-1)].

% @private
% Puts each elemet of List in front of the list in LoL at the same position.
assign_tuple(List,LoL) ->
    assign_tuple(List,LoL,[]).

assign_tuple([],[],Acc) ->
    lists:reverse(Acc);
    
assign_tuple(List,LoL,Acc) ->
    L=hd(List),
    O=hd(LoL),
    assign_tuple(tl(List),tl(LoL),[[L|O]|Acc]).
    
    

-type deep_list(A) ::[ A | deep_list(A)].
-spec deep_map(fun((A) -> B),List::deep_list(A)) -> deep_list(B).

% @doc Maps a function to all elements of a deep list.
%
% <code>
% > {@module}:deep_map(fun(X) -> X + 1 end, [[1,[2]],3]).<br/>
%  [[2,[3]],4]<br/>
% </code>
deep_map(Fun,[A|B]) ->
[deep_map(Fun,A)|deep_map(Fun,B)];

deep_map(_Fun,[]) -> [];

deep_map(Fun,A) -> Fun(A).

-spec deep_foldl(fun((A,Acc) -> Acc),Acc,List::deep_list(A)) -> Acc.

% @doc Folds  a function over all elements of a deep list.
deep_foldl(Fun,Acc,[A|B]) ->
    deep_foldl(Fun,deep_foldl(Fun,Acc,A),B);

deep_foldl(_Fun,Acc,[]) -> Acc;

deep_foldl(Fun,Acc,A) -> Fun(A,Acc).

-spec deep_mapfoldl(fun((A,Acc) -> {B,Acc}),Acc,List::deep_list(A)) -> {deep_list(B),Acc}.
% @doc A {@link lists:mapfoldl/3} over a deep list

deep_mapfoldl(Fun,Acc,[A|As]) ->
    {B,Bcc} = deep_mapfoldl(Fun,Acc,A),
    {C,Ccc} = deep_mapfoldl(Fun,Bcc,As),
    {[B | C],Ccc};

deep_mapfoldl(_Fun,Acc,[]) -> {[],Acc};

deep_mapfoldl(Fun,Acc,A) -> Fun(A,Acc).

% TODO: Design and implement funtions that does ([a],[b,a,c]) -> [b,c,a]/[a,b,c] instead of ([a],[b,a,c]) -> [b,a,c].


-spec keyumerge(N::pos_integer(),[A::tuple()],[B::tuple()]) %when N =< size(A) andalso size(A)==size(B) 
-> [tuple()].

% @doc Makes a merge between two tuple lists.
% Unlike lists:ukeymerge/3, the resulting list will only contain one element for each key, even if one of the source lists contain several.
%
% If two elements have the same key, the first element with the key from the first list is used.
%
% <code>
% > {@module}:keyumerge(1,[{a,b},{a,d}],[{a,c},{b,c}]).<br/>
% [{b,c},{a,b}]<br/>
% </code>
%
keyumerge(N,A,B) ->
  lists:reverse(keyumerge(N,keyuniq(N,A),keyuniq(N,B),[])).

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

-spec keyudmerge(N::pos_integer(),[A::tuple()],[B::tuple()]) %when N =< size(A) andalso size(A)==size(B) 
    -> [tuple()].

% @doc Makes a distinct, merge between two sorted distinct tuple lists.
%
% If two elements have the same key, the element from the first list is used.
%
keyudmerge(N,A,B) ->
  keyudmerge(N,A,B,[],[]).

keyudmerge(_N,[],[],Acc,Bcc) ->
  {Acc,lists:reverse(Bcc)};

keyudmerge(_N,L1,[],Acc,Bcc) ->
  {lists:reverse(Acc)++L1,lists:reverse(Bcc)};

keyudmerge(_N,[],L2,Acc,Bcc) ->
  {lists:reverse(Acc)++L2,lists:reverse(Bcc)};

keyudmerge(N,LL1=[I1|L1],LL2=[I2|L2],Acc,Bcc) ->
  A=element(N,I1),
  B=element(N,I2),
  if 
    A == B ->
      keyudmerge(N,L1,L2,[I1|Acc],[I2|Bcc]);
    A < B ->
      keyudmerge(N,L1,LL2,[I1|Acc],Bcc);
    A > B ->
      keyudmerge(N,LL1,L2,[I2|Acc],Bcc)
  end.

% @private
% @deprecated
% @doc seems to be doing the exact same thing as keyudmerge/3. Why did I build this?
unsort_keymerge(_N,[],B) -> {B,[]};

unsort_keymerge(N,A,B) ->
    unsort_keymerge(N,lists:reverse(A),B,[]).

unsort_keymerge(_N,A,[],Bcc) ->
    {lists:reverse(A),lists:reverse(Bcc)};

unsort_keymerge(N,A,B,Bcc) ->
    Item=hd(B),
    case lists:keymember(element(N,Item),N,A) 
        of true ->
            unsort_keymerge(N,A,tl(B),[Item|Bcc])
        ; false ->
            unsort_keymerge(N,[Item|A],tl(B),Bcc)
    end.

            



% @doc Only keeps one of each element.
% Comparison by {@link erlang:'=='/2}.
uniq(L) -> uniq(lists:sort(L),[]).

uniq([],Bs) -> lists:reverse(Bs);
uniq([A|As],[B|Bs]) when A == B -> uniq(As,[B|Bs]);
uniq([A|As],Bs) -> uniq(As,[A|Bs]).

-spec keyuniq(pos_integer(),[tuple()]) -> [tuple()].
% @doc Only keeps one of each element with the key at position Key.
% Comparison on key by {@link erlang:'=='/2}.
keyuniq(Key,L) -> skeyuniq(Key,lists:keysort(Key,L),[]).

% @doc Only keeps one of each element with the key at position Key. Does not sort the list beforehand.
% Uniqueness is only guaranteed per group. Unless keys are already grouped together, this will result in several groups with the same key.
% Comparison by {@link erlang:'=='/2}.
skeyuniq(Key,L) -> skeyuniq(Key,L,[]).

skeyuniq(_Key,[],Bs) -> lists:reverse(Bs);
skeyuniq(Key,[A|As],[B|Bs]) when element(Key,A) == element(Key,B) -> skeyuniq(Key,As,[B|Bs]);
skeyuniq(Key,[A|As],Bs) -> skeyuniq(Key,As,[A|Bs]).


% @doc unique insert
%
% Insert an element into a list, unless it is already there.

-spec uinsert(A,List::[A]) -> [A].

uinsert(A,Bs) ->
    case is_in(A,Bs) of
        true -> Bs;
        false -> [A|Bs]
    end.

% @doc unique map insert
%
% Insert all of the elements from the first list into the second that are not already there.
-spec uminsert([A],[A]) -> [A].
uminsert(As,Bs) ->
    lists:foldr(fun uinsert/2,Bs,As).


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

-spec is_in(A,List::[A]) -> boolean().
% @doc Basically {@link lists:member/2} with a nicer name.
is_in(_A,[]) -> false;

is_in(A,[B|Bs]) ->
    if A==B -> true;
        true -> is_in(A,Bs)
    end.



% @doc Picks all tuples that match any of the keys in the keylist.
%
% Takes out all tuples from KeyVals that has a key in Keys at tuple position KeyPos.
%
%
% <code>
% > {@module}:select_keyvals([a],2,[{a,x,z},{b,a},{c,a,w}]).<br/>
% [{b,a},{c,a,w}]<br/>
% </code>

-spec select_keyvals([any()],pos_integer(),[tuple()]) -> [tuple()].
select_keyvals(Keys,KeyPos,KeyVals) ->
    lists:filter(fun(X) -> lists:member(element(KeyPos,X),Keys) end,KeyVals).

% @doc Picks all values that matches the keys in the keylist.
%
% Takes out the value at position ValPos from all tuples from KeyVals whose key at position KeyPos is in Keys.
% 
% Crashes if the value position is outside the tuple boundaries.
%
%
% <code>
% > {@module}:select_vals([a],2,1,[{a,x,z},{b,a},{c,a,w}]).<br/>
% [b,c]<br/>
% </code>
-spec select_vals(list(any()),pos_integer(),pos_integer(),list(tuple())) -> Vals::list(any()).
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
% <code>
% > {@module}:sift(fun(X) -> X == 3 end,[1,3,4]).<br/>
% {[3],[1,4]}</code>
% @equiv lists:partition(F,L)
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
%
% <code>
% > {@module}:siftmap(fun(X) -> if X == 3 -> {true,a}; true -> false end,[1,3,4]).<br/>
% {[a],[1,4]}<br/>
% </code>
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


% @doc Pairs elements with their matching values, according to a match function. Returns a tuple of the matches and unmatched values.
%
% Essentially, combines a sift with a {@link lists:mapfoldl/3}.
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
%
% <code>
% > {@module}:mapsiftl([2,3,4],fun(X) -> fun(Y) -> Y rem X == 0 end end,lists:seq(1,10)).<br/>
% {[{2,[2,4,6,8,10]},{3,[3,9]},{4,[]}],[1,5,7]}<br/>
% </code>
-spec mapsiftl(A::[X],B::constructor(X,sorting_fun(Y)),C::[Y]) -> {[{X,[Y]}],[Y]}.
mapsiftl(Items,Pred,List) ->
    lists:mapfoldl(
        fun(Item,SubList) ->
                {Res,Rest} = sift(Pred(Item),SubList),
                {{Item,Res},Rest}
        end,
        List,
        Items).

% @doc The same as mapsiftl, but with the option to change the matching elements.
%
%
% <code>
% > {@module}:mapsiftmapl([2,3,4],fun(X) -> fun(Y) -> if Y rem X == 0 -> {true,x}; true -> false end end end,lists:seq(1,10)).<br/>
%   {[{2,[x,x,x,x,x]},{3,[x,x]},{4,[]}],[1,5,7]}<br/>
% </code>
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
% <code>
% > {@module}:sequences(3,[1,2,3,4,5]).<br/>
% [[1,2,3],[2,3,4],[3,4,5]]<br/> 
% </code>
-spec sequences(pos_integer(),List::[X]) -> [[X]].
sequences(Length,List) when Length > length(List) ->
    [];

sequences(Length,List) ->
    [pick_n(Length,List)|sequences(Length,tl(List))].

% @doc Runs {@link sequence/2} on the list, and runs {@link split_at/2} on each resulting sublist. 
%
% <code>
% > {@module}:sequence_split(3,2,List=lists:seq(1,100)).<br/>
% [{[1,2],[3]},{[2,3],[4]}...]<br/>
% </code>
-spec sequence_split(pos_integer(),pos_integer(),List::[X]) -> [{[X],[X]}].
sequence_split(SequenceLength,SplitAt,List) ->
    Split = fun(SubList) -> 
            split_at(SplitAt,SubList) 
    end,
    lists:map(Split,sequences(SequenceLength,List)).

% @doc splits a list at a certain point.
% @deprecated
% @equiv lists:split(N,List)
% @end
% , length(L) ==N.
-spec split_at(non_neg_integer(),List::[X]) -> {[X],[X]}
    when X :: any().
split_at(N,List) ->
    {pick_n(N,List),lists:nthtail(N,List)}.


% @doc Picks an element at random, and returns it discarding the rest of the list.
-spec select_one(List::[X]) -> X.
select_one(List) ->
    lists:nth(rand:uniform(length(List)),List).

% @doc Picks the nth element of a list, and returns it together with the rest of the list.
% Returns false if no such element exists.
-spec pick_nth(List::[X],pos_integer()) -> {X,[X]}.

pick_nth(_,N) when N =< 0 -> false;
pick_nth(L,N) when N > length(L) -> false;

pick_nth(List,N) ->
    pick_nth(List,N,[]).

pick_nth([L|Ls],1,Before) -> {L,(lists:reverse(Before))++Ls};
pick_nth([L|Ls],N,Before) -> pick_nth(Ls,N-1,[L|Before]).


% @doc The same as pick_nth, but does not reverse its arguments on return.
% For efficiency reasons.
-spec pick_nth_nonrev(List::[X],pos_integer()) -> {X,[X]}.
pick_nth_nonrev(_,N) when N =< 0 -> false;
pick_nth_nonrev(L,N) when N > length(L) -> false;

pick_nth_nonrev(List,N) ->
    pick_nth_nonrev(List,N,[]).

pick_nth_nonrev([L|Ls],1,Before) -> {L,Before++Ls};
pick_nth_nonrev([L|Ls],N,Before) -> pick_nth_nonrev(Ls,N-1,[L|Before]).

% @doc Pick an element at random from the list, and return the element together with the rest of the list.
%
% The order of the list is not guaranteed to be maintained.

-spec pick_random(List::[X]) -> {Picked::X,Rest::[X]}.
pick_random(List) ->
    pick_nth_nonrev(List,rand:uniform(length(List))).

% @doc pick N elements at random from the list, and return those elements together with the rest of the list.
-spec pick_n_random(non_neg_integer(),List::[X]) -> {Picked::[X],Rest::[X]}
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
                {Nth,List1}=pick_nth(As,rand:uniform(length(As))),
                {List1,[Nth|Acc]};
            (_,As) ->
                {Nth,List1}=pick_nth(As,rand:uniform(length(As))),
                {List1,[Nth]}
        end,
        List,
        List).

% @doc picks the first N elements from a list. 
%
% Crashes if N is bigger than the length of the list.
%
% <code>
% > {@module}:pick_n(3,[1,2,3,4]).<br/>
% [1,2,3]<br/>
% > {@module}:pick_n(3,[1,2]). % throws an exception.
% </code>
-spec pick_n(Index::non_neg_integer(),List::[X]) -> [X] when X :: any().
pick_n(A,B) -> pick_n_(A,B,[]).

pick_n_(0,_,Acc) -> lists:reverse(Acc);
pick_n_(N,[L|Ls],Acc) -> pick_n_(N-1,Ls,[L|Acc]).

% @doc Non-crashing version of {@link pick_n/2}.
%
% Returns undef for any element that would have been inserted had the list been long enough.
%
% <code>
% > {@module}:pick_n_undef(3,[1,2,3,4]).<br/>
% [1,2,3]<br/>
% > {@module}:pick_n_undef(3,[1,2]).<br/>
% [1,2,undef]<br/>
% </code>
% @equiv pick_n(Index,List,undef)

-spec pick_n_undef(Index::non_neg_integer(),List::[X]) -> [X|undef].
pick_n_undef(A,B) -> pick_n_undef(A,B,[]).
pick_n_undef(0,_,Acc) -> lists:reverse(Acc);
pick_n_undef(N,[L|Ls],Acc) -> pick_n_undef(N-1,Ls,[L|Acc]);
pick_n_undef(N,[],Acc) -> pick_n_undef(N-1,[],[undef|Acc]).

% @doc Non-crashing version of {@link pick_n/2}, with a default value.
%
% Returns the default value for any element that would have been inserted had the list been long enough.
%
% <code>
% > {@module}:pick_n(3,[1,2,3,4],kaka) <br/>
% [1,2,3]<br/>
% > {@module}:pick_n(3,[1,2],kaka) <br/>
% [1,2,kaka]
% </code>
-spec pick_n(Index::non_neg_integer(),List::[X],Default) -> [X|Default].
pick_n(A,B,Y) -> pick_n_(A,B,Y,[]).

pick_n_(0,_,_Y,Acc) -> lists:reverse(Acc);
pick_n_(N,[L|Ls],_Y,Acc) -> pick_n_(N-1,Ls,[L|Acc]);
pick_n_(N,[],Y,Acc) -> pick_n_(N-1,[],[Y|Acc]).

% @doc pick some elements from a list
%
% Returns the element from the second list whose index are in the first list.

-spec pick_elems([pos_integer()],List::[A]) -> [A].
pick_elems(Elems,List) -> lists:map(fun(N) -> lists:nth(N,List) end,Elems).


group_(Thing,false) when is_list(Thing) ->
    {Key,Other}=lst_ext:pick_nth(Thing,1),
    {Key,[Other],[]};
    
group_(Thing,{OldKey,Acc,Bcc}) when is_list(Thing) ->
    {Key,Other}=lst_ext:pick_nth(Thing,1),
    if
        Key==OldKey ->
            {OldKey,[Other|Acc],Bcc};
        true ->
            {Key,[Other],[{OldKey,lists:reverse(Acc)}|Bcc]}
    end.

-spec group([[X|Y]]) ->  [{X,[[Y]]}].
% @doc Groups together into a tuple all adjacent lists with the same first element 
%
% Example:
%
% <code>
% > {@module}:lst_ext:group([[a,b,c],[a,b,d],[b,c],[a,c]]).<br/>
% [{a,[[c]]},{b,[[c]]},{a,[[b,c],[b,d]]}]
% </code>
group(LoL) ->
    {LastKey,LastAcc,Bcc}=lists:foldl(fun group_/2,false,LoL),
    [{LastKey,LastAcc}|Bcc].
    

group_by_nth_(Elem) ->
    fun 
        (Thing,false) when is_list(Thing) ->
            {Key,Other}=lst_ext:pick_nth(Thing,Elem),
            {Key,[Other],[]};
        (Thing,{OldKey,Acc,Bcc}) when is_list(Thing) ->
            {Key,Other}=lst_ext:pick_nth(Thing,Elem),
            if
                Key==OldKey ->
                    {OldKey,[Other|Acc],Bcc};
                true ->
                    {Key,[Other],[{OldKey,Acc}|Bcc]}
            end
    end.

-spec group_by_nth(Index::non_neg_integer(),[[X|Y]]) ->  [{X,[Y]}].
% @doc Groups together into a tuple all adjacent lists with the same nth element 
%
% Example:
%
% <code>
% > {@module}:lst_ext:group_by_nth(2,[[a,b,c],[a,b,d],[b,c],[a,c]]).<br/>
% [{b,[[a,d],[a,c]]},{c,[[b],[a]]}]
% </code>
group_by_nth(Elem,LoL) ->
    Fun = group_by_nth_(Elem),
    {LastKey,LastAcc,Bcc}=lists:foldl(Fun,false,LoL),
    lists:reverse([{LastKey,lists:reverse(LastAcc)}|Bcc]).

-spec set_equiv([any()],[any()]) -> boolean().
% @doc Tests whether two lists are equivalent when considered as sets
set_equiv(L1,L2) ->
    S1=sets:from_list(L1),
    S2=sets:from_list(L2),
    sets:is_subset(S1,S2) andalso sets:is_subset(S2,S1).

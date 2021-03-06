% @doc This module contains various statistics functions.
-module(vol_stat).

-export([
    std/1,
    avg/1,
    sse/2,
    mse/2,
    sae/2,
    mae/2,
    ae/2,
    rms/2
    ]).
    


% @doc  Returns the average of a list
% avg([1,2,3]) -> 2.0
avg(List)->
	lists:sum(List)/length(List).

% @doc  Returns the standard deviation of a list
std(List)->
	Avg = avg(List),
	std(List,Avg,[]).
	
std([Val|List],Avg,Acc)->
    std(List,Avg,[math:pow(Avg-Val,2)|Acc]);

std([],_Avg,Acc)->
    Variance = lists:sum(Acc)/length(Acc),
    math:sqrt(Variance).

% @doc sum of square of errors
sse(As,Bs) -> lists:sum(sse(As,Bs,[])).

sse([],[],Acc) ->
    Acc;

sse([A|As],[B|Bs],Acc) ->
    sse(As,Bs,[math:pow(A-B,2)|Acc]).

% @doc mean square error between the two lists
mse(As,Bs) ->
    math:sqrt(lists:sum(sse(As,Bs,[]))/length(As)).

% @doc sum of absolute errors of the two listes
sae(As,Bs) -> lists:sum(ae(As,Bs,[])).

% @doc a list of all absolute errors between corresponding elements of the two lists.
ae(As,Bs) -> ae(As,Bs,[]).

ae([],[],Acc) ->
   Acc;

ae([A|As],[B|Bs],Acc) ->
    ae(As,Bs,[abs(A-B)|Acc]).

% @doc mean average error between the two lists
mae(As,Bs) ->
    avg(ae(As,Bs,[])).

%%--------------------------------------------------------------------
%% @doc calculates the root mean square of the element wise difference of the lists.
%%
%% @spec (number(),number()) -> number()
%% @end
%%--------------------------------------------------------------------
%% XXX: Needs to test for inf in sse return
rms(Foo,Bar) -> math:sqrt(?MODULE:sse(Foo,Bar)/length(Bar)). 

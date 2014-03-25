-module(vol_misc).

-export([restart/1
         ,chain/1
         ,rechain/1
         ,recompile/0
         ,recompile/1
         ,reload_files/1
         ,dispatch/1
         ,fst/1
         ,flip/1
         ,snd/1
         ,make_documentation/1
         ,make_documentation/0
         ,number_to_string/1
         ,fix_home/1
         ,recompile_live/0
         ,fac/1
         ,fac_plus/2
         ,nCr/2
         ,nCr_exact/2
         ,reload_app/1
         ,gen_id/0
        ]).


% @doc generates a unique id to be used for whatever.
gen_id() -> now().

restart(Module) ->
    application:stop(Module),
    application:unload(Module),
    application:start(Module).

% Why is this not in any standard lib‽
fac(0) -> 1;
fac(N) -> N*fac(N-1).

fac_plus(0,_M) -> 1;
fac_plus(1,_M) -> 1;
fac_plus(N,M) -> (N+M)*fac_plus(N-1,M).


%nCr2(N,K) -> fac_plus(K,N-K) div fac(K).

nCr_exact(N,K) ->
    nCr_exact(N,K,1,1).

nCr_exact(_N,0,NAcc,KAcc) -> NAcc div KAcc;
nCr_exact(N,K,NAcc,KAcc) -> nCr_exact(N-1,K-1,NAcc*N,KAcc*K).

nCr(N,K) ->
    nCr(N,K,1).

nCr(_N,0,Acc) -> Acc;
nCr(N,K,Acc) -> nCr(N-1,K-1,N/K*Acc).


make_documentation() ->
    {ok,Files} = file:list_dir(ebin),
    Relevant_files=lists:filter(fun(X) -> lists:suffix(".app",X) end,Files),
    Stripped_files=lists:map(fun(X) -> string:substr(X,1,length(X)-4) end,Relevant_files),
    lists:map(fun make_documentation/1,Stripped_files).

make_documentation(Application) when is_atom(Application)->
    edoc:application(Application,'.',[{dir,"doc/"++atom_to_list(Application)}]);

make_documentation(Application) when is_list(Application)->
    edoc:application(list_to_atom(Application),'.',[{dir,"doc/"++Application}]).

chain(Module) ->
    case application:start(Module) of
        {error,{not_started,A}} ->
            chain(A),
            chain(Module);
        A -> A
    end.


rechain(Module) ->
    case restart(Module) of
        {error,{not_started,A}} ->
            rechain(A),
            rechain(Module);
        A -> A
    end.

fst(Tuple) -> element(1,Tuple).
snd(Tuple) -> element(2,Tuple).

fix_home([$~,$/|Name]) ->
    [os:getenv("HOME"),"/",Name];

fix_home(A) -> A.

%Can this be implemented as a function? 
% f(I),I=fun() -> f(F),f(G),f(H),{ok,F}=file:list_dir("src/"),G=lists:filter(fun([$\.|_]) -> false; (A) -> (string:rstr(A,".orig")==0) end,F),H=lists:map(fun(X) -> l(list_to_atom(string:substr(X,1,string:len(X)-4))) end,G),io:format("~p",[H]) end, (make:all([{d,timestamp},{d,debug},{d,test},debug_info])==up_to_date) andalso I().
recompile()->
    recompile("src/").

recompile(Dir)->
    {ok,Files}=file:list_dir(Dir),
    (make:all()==up_to_date) andalso 
        reload_files(valid_files(Files)).

recompile_live() ->
    recompile_live("src/").

recompile_live(Dir) ->
    {ok,Files}=file:list_dir(Dir),
    (make:all()==up_to_date) andalso 
        reload_files_live(valid_files(Files)).

valid_files(Files) ->
    lists:filter(fun
                    ([$\.|_]) -> false; 
                    (A) -> (string:rstr(A,".erl")==length(A)-3) 
                    %(A) -> lists:suffix(A,".erl")
                end,
                Files).

reload_files(Files) ->
    lists:map(fun(X) ->                                                                                                                                                            
                  Name=list_to_atom(string:substr(X,1,string:len(X)-4)),
                  code:purge(Name),
                  code:load_file(Name)
              end,
              Files).


reload_files_live(Files) ->
    lists:map(fun(X) ->
                  Name=list_to_atom(string:substr(X,1,string:len(X)-4)),
                  case whereis(Name) of
                      undefined ->
                          code:purge(Name),
                          code:load_file(Name);
                      _Defined ->
                          sys:suspend(Name),
                          code:purge(Name),
                          code:load_file(Name),
                          sys:change_code(Name,Name,automagic,[]),
                          sys:resume(Name)
                  end
              end,
              Files).


reload_app(Dir) ->
    case file:list_dir(Dir++"/src/") of
        {ok,Files} ->
            % Nödlösning med flatten. Kan fixas via snygga algoritmer. Sen™.
            lists:flatten(
                [{Dir,reload_files(valid_files(Files))}|
                  case file:list_dir(Dir++"/deps/") of
                        {error,enoent} -> 
                            [];
                        {ok,Dirs} ->
                            Deps = lists:map(fun(X) -> Dir++"/deps/"++X end,Dirs),
                            lists:map(fun reload_app/1,Deps)
                  end
                          ]);
        {error,enoent} ->
            {Dir,enoent}
    end.



flip([]) -> [];
flip([{A,B}|ListOfTuples]) ->[{B,A}|flip(ListOfTuples)].

dispatch(Prog) ->
    systools:make_script(Prog,[local]),
    systools:make_tar(Prog,[{erts, "/usr/lib/erlang"}]).

number_to_string(Number) when Number < 10 -> [48+Number];
number_to_string(Number) -> number_to_string(trunc(Number/10))++[ 48+(Number rem 10)].


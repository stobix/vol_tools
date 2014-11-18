% @doc This module contains various functions that doesn't fit in any of the categories encompassed by the other modules.
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
         ,mkdoc_deps/0
         ,number_to_string/1
         ,fix_home/1
         ,recompile_live/0
         ,fac/1
         ,fac_plus/2
         ,nCr/2
         ,nCr_exact/2
         ,reload_app/1
         ,gen_id/0
         ,microseconds_to_hms/1
         ,callback/1
         ,callback/2
         ,thing_to_number/1
        ]).


thing_to_number(X) when is_number(X) -> X;

thing_to_number(X) when is_list(X) ->
    try list_to_integer(X) of
        Y -> Y
    catch
        error:badarg ->
            list_to_float(X) 
    end;
        
thing_to_number(X) when is_atom(X) -> thing_to_number(atom_to_list(X));

thing_to_number(_) -> error(badarg).
        

% @doc Calls a callback function/process
%
% Valid callback types and what they do:
% <ul>
% <li>
% <code>
% none
% </code>
% </li> Does nothing
% <li>
% <code>
% pid()
% </code>
% </li> sends the atom 'done' to the pid.
% <li>
% <code>
% {pid(),Thing}
% </code>
% </li> sends Thing to the pid
% <li>
% <code>
% {Function,Argument}
% </code>
% </li> Spawns a process calling the function with the argument
% <li>
% <code>
% Function
% </code>
% </li> Spawns a process calling the function.
% </ul>
-spec callback(none | pid() | {pid(),any()} | {fun( (A) -> any() ), A} | fun( () -> any() )) -> no_return().
callback(Callback) ->
    case Callback of
        none ->
            none;
        A when is_pid(A) ->
            A ! done;
        {A,B} when is_pid(A) ->
            A ! B;
        {A,B} ->
            spawn(fun() -> A(B) end);
        A ->
            spawn(fun() -> A() end)
    end.

% @doc Calls a callback function/process with a message
%
% Valid callback types and what they do:
% <ul>
% <li>
% <code>
% none
% </code>
% </li> Does nothing
% <li>
% <code>
% pid()
% </code>
% </li> sends Msg to the pid.
% <li>
% <code>
% {pid(),Thing}
% </code>
% </li> sends {Msg,Thing} to the pid
% <li>
% <code>
% {Function,Argument}
% </code>
% </li> Spawns a process calling the function with Msg as the first argument, and Argument as the second.
% <li>
% <code>
% Function
% </code>
% </li> Spawns a process calling the function with the Msg as argument.
% </ul>
-spec callback(none | pid() | {pid(),any()} | {fun( (A,B) -> any() ), A} | fun( () -> any() ),Msg::B) -> no_return().
callback(Callback,Msg) ->
    case Callback of
        none ->
            none;
        A when is_pid(A) ->
            A ! Msg;
        {A,B} when is_pid(A) ->
            A ! {Msg,B};
        {A,B} ->
            spawn(fun() -> A(Msg,B) end);
        A ->
            spawn(fun() -> A(Msg) end)
    end.

microseconds_to_hms(MicroTime) ->
    Time=MicroTime/1000,
    Hours = trunc(Time/timer:hours(1)),
    DiffTime1=  Time - (Hours*timer:hours(1)),
    Minutes = trunc(DiffTime1/timer:minutes(1)),
    DiffTime2 = DiffTime1 - (Minutes*timer:minutes(1)),
    Seconds = trunc(DiffTime2/timer:seconds(1)),
    {Hours,Minutes,Seconds}.

% @doc generates a unique id for arbitrary use.
gen_id() -> now().

% @doc Restart an application
restart(Module) ->
    application:stop(Module),
    application:unload(Module),
    application:start(Module).

% @doc A simple faculty function.
% Why is this not in any standard lib‽
-spec fac(non_neg_integer()) -> pos_integer().
fac(0) -> 1;
fac(N) -> N*fac(N-1).

% @doc A faculty function where each element has a value added to it before multiplying together.
%
% fac_plus(N,M) = fac(N+M)/fac(M)
-spec fac_plus(non_neg_integer(),integer()) -> integer().
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


% @doc Find all applications with an .app file in our .ebin directory, and make documentation for them.
make_documentation() ->
    {ok,Files} = file:list_dir(ebin),
    Relevant_files=lists:filter(fun(X) -> lists:suffix(".app",X) end,Files),
    Stripped_files=lists:map(fun(X) -> string:substr(X,1,length(X)-4) end,Relevant_files),
    lists:map(fun make_documentation/1,Stripped_files).
% @doc Generates documentation for an app, and put the documentation into doc/appname
make_documentation(Application) when is_atom(Application)->
    edoc:application(Application,'.',[{dir,"doc/"++atom_to_list(Application)}]);

make_documentation(Application) when is_list(Application)->
    edoc:application(list_to_atom(Application),'.',[{dir,"doc/"++Application}]).

mkdoc_deps() ->
    {ok,Deps} = file:list_dir(deps),
    lists:foreach(fun mkdoc_deps/1,Deps),
    make_documentation().
      
mkdoc_deps(Dep) ->
    spawn(fun() ->edoc:application(list_to_atom(Dep),"deps/"++Dep,[{dir,"doc/deps/"++Dep}]) end).


% @doc Starts an app, ensuring that all its dependencies are started.
%
% This has the same effect as application:ensure_all_started/1 in erts 17.0 or above. In lower versions, it is not present.
chain(Module) ->
    case application:start(Module) of
        {error,{not_started,A}} ->
            chain(A),
            chain(Module);
        A -> A
    end.


% @doc Restarts an app, ensuring that all its dependencies are "rechained" unless they're already started.
rechain(Module) ->
    case restart(Module) of
        {error,{not_started,A}} ->
            rechain(A),
            rechain(Module);
        A -> A
    end.

% @doc Pick the first element from a tuple. Good for use in maps, e.g. list:map(fun vol_misc:fst/1,Things) instead of the (slightly) longer lists:map(fun (X) -> element(1,X) end,Things).
fst(Tuple) -> element(1,Tuple).
% @doc Pick the second element from a tuple. Good for use in maps.
snd(Tuple) -> element(2,Tuple).

% @doc Replaces any leading $~ with the current user's home directory.
fix_home([$~,$/|Name]) ->
    [os:getenv("HOME"),"/",Name];

fix_home(A) -> A.

%Can this be implemented as a function? 
% f(I),I=fun() -> f(F),f(G),f(H),{ok,F}=file:list_dir("src/"),G=lists:filter(fun([$\.|_]) -> false; (A) -> (string:rstr(A,".orig")==0) end,F),H=lists:map(fun(X) -> l(list_to_atom(string:substr(X,1,string:len(X)-4))) end,G),io:format("~p",[H]) end, (make:all([{d,timestamp},{d,debug},{d,test},debug_info])==up_to_date) andalso I().
% @doc Recompiles and reloads all files in "./src".
recompile()->
    recompile("src/").

% @doc Recompiles and reloads all files in Dir.
recompile(Dir)->
    {ok,Files}=file:list_dir(Dir),
    (make:all()==up_to_date) andalso 
        reload_files(valid_files(Files)).

% @doc Recompiles and reloads all files in "./src", updating all recompiled and running modules.
recompile_live() ->
    recompile_live("src/").

% @doc Recompiles and reloads all files in Dir, updating all recompiled and running modules.
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


% @doc reload all files from all dependencies of the OTP app residing in dir.
%
% More specifically, this tries to reload all modules corresponding to all erl files in all src directories in the deps directory/ies and ., recursively.
% Returns a list of successfully reloaded modules.

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


% @doc Flips the first and second position in each tuple in a list of 2-tuples.
flip([]) -> [];
flip([{A,B}|ListOfTuples]) ->[{B,A}|flip(ListOfTuples)].

dispatch(Prog) ->
    systools:make_script(Prog,[local]),
    systools:make_tar(Prog,[{erts, "/usr/lib/erlang"}]).

% @deprecated This is essentially integer_to_list/1, which is probably better defined anyways.
number_to_string(Number) when is_integer(Number) -> integer_to_list(Number);
number_to_string(Number) when is_float(Number) -> io_lib:format("~p",[Number]).


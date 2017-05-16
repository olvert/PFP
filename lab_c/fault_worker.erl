-module(fault_worker).
-compile(export_all).

%% Worker pool with fault tolerance %%

spawn_workers() ->
  Workers = [spawn_workers_on_node(S) || S <- get_schedulers()],
  lists:flatten(Workers).

kill_workers(Workers) ->
  [unlink(W) || W <- Workers],
  [exit(W,kill) || W <- Workers].

spawn_pool() ->
  yes = global:register_name(master, self()),
  spawn_link(fun()-> init_assign() end).

kill_pool(Workers) ->
  kill_workers(Workers),
  global:unregister_name(pool),
  global:send(master, {pool, stopped}). 

init_assign() ->
  yes = global:register_name(pool, self()),
  global:send(master, {pool, registered}),
  process_flag(trap_exit,true),
  Workers = spawn_workers(),
  assign_tasks([], maps:new(), Workers). 

assign_tasks([], Map, Workers) ->
  receive
    {tasks, Tasks} ->
      assign_tasks(Tasks, Map, Workers);
      
    {'EXIT',Pid,_} -> 
      reassign(Pid, [], Map, Workers);
  
    {stop} -> 
      kill_pool(Workers)
  end;

assign_tasks([{F, Ref}|Tasks], Map, Workers) ->
  receive
    {available, Pid} ->
      NewMap = maps:put(Pid, {F, Ref}, Map),
      Pid ! {task, F, Ref},
      assign_tasks(Tasks, NewMap, Workers);
    
    {'EXIT',Pid,_} -> 
      reassign(Pid, [{F, Ref}|Tasks], Map, Workers);
  
    {stop} -> 
      kill_pool(Workers)
  end.

reassign(Pid, Tasks, Map, Workers) ->
  {ok, Task} = maps:find(Pid, Map),
  NewMap = maps:remove(Pid, Map),
  assign_tasks([Task|Tasks], NewMap, Workers).
    
spawn_workers_on_node({Node, N}) ->
  [worker(Node) || _ <- lists:seq(1, N)].

worker(Node) ->
  spawn_link(Node, fault_worker, work, []).

work() ->
  Pid = self(),
  global:send(pool, {available, Pid}),
  receive
    {task, F, Ref} ->
      global:send(master, {result, Ref, F()}),
      work()
  end.

worker_pool(Funs) ->
  Tasks = [{F, make_ref()} || F <- Funs],
  global:send(pool, {tasks, Tasks}),
  Results = [recieve_result(Ref) || {_, Ref} <- Tasks],
  Results.  

recieve_result(Ref) ->
  receive
    {result, Ref, Res} ->
      Res
  end.

%% Get number of available schedulers from each node
get_schedulers() ->
  Self = {node(), erlang:system_info(schedulers)-2},
  Others = [ {Node, rpc:call(Node, erlang, system_info, [schedulers])} 
           || Node <- nodes()],
  [Self | Others].  

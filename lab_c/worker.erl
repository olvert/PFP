-module(worker).
-compile(export_all).

%% Worker pool %%

spawn_workers() ->
  Master  = self(),
  Workers = [spawn_workers_on_node(Master, S) || S <- get_schedulers()],
  lists:flatten(Workers).

kill_workers(Workers) ->
  [unlink(W) || W <- Workers],
  [exit(W,kill) || W <- Workers].

flush() ->
  receive
    _ -> flush()
  after
    0 -> ok
  end.
    
spawn_workers_on_node(Master, {Node, N}) ->
  [worker(Master, Node) || _ <- lists:seq(1, N)].

worker(Master, Node) ->
  spawn_link(Node, worker, work, [Master]).

work(Master) ->
  Pid = self(),
  Master ! {available, Pid},
  receive
    {task, F} ->
      Master ! {result, Pid, F()},
      work(Master)
  end.

worker_pool(Funs) ->
  Pids    = [assign_task(F) || F <- Funs],
  Results = [recieve_result(Pid) || Pid <- Pids],
  Results.  
  
assign_task(F) ->
  receive
    {available, Pid} ->
      Pid ! {task, F}
  end,
  Pid.

recieve_result(Pid) ->
  receive
    {result, Pid, Res} ->
      Res
  end.

%% Get number of available schedulers from each node
get_schedulers() ->
  Self = {node(), erlang:system_info(schedulers)-1},
  Others = [ {Node, rpc:call(Node, erlang, system_info, [schedulers])} 
           || Node <- nodes()],
  [Self | Others].  

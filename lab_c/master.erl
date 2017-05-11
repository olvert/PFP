-module(master).
-compile(export_all).


%% Solution 1: Distributing Map-Reduce %%

map_reduce_dist(Map,M,Reduce,R,Input) ->
  Parent   = self(),
  Splits   = map_reduce:split_into(M,Input),
  Mappers  = spawn_mappers(all_nodes(), Parent, Map, R, Splits),
  Mappeds  = [receive {Pid,L} -> L end || Pid <- Mappers],
  Indices  = lists:seq(0,R-1),
  Reducers = spawn_reducers(all_nodes(), Parent, Reduce, Mappeds, Indices),
  Reduceds = [receive {Pid,L} -> L end || Pid <- Reducers],
  lists:sort(lists:flatten(Reduceds)).

spawn_mappers(_, _, _, _, []) ->
  [];
spawn_mappers([N|Ns], Parent, Map, R, [S|Ss]) ->
  M  = spawn_mapper(N, Parent, Map, R, S),
  Ms = spawn_mappers(Ns++[N], Parent, Map, R, Ss),
  [M|Ms].
  
spawn_mapper(Node, Parent,Map,R,Split) ->
  spawn_link(Node, fun() ->
		Mapped = get_mapped(Map, R, Split),
		Parent ! {self(),map_reduce:group(lists:sort(Mapped))}
	end).

spawn_reducers(_, _, _, _, []) ->
  [];
spawn_reducers([N|Ns], Parent, Reduce, Mappeds, [I|Is]) ->
  R  = spawn_reducer(N, Parent, Reduce, I, Mappeds),
  Rs = spawn_reducers(Ns++[N], Parent, Reduce, Mappeds, Is),
  [R|Rs].

spawn_reducer(Node,Parent,Reduce,I,Mappeds) ->
  Inputs = get_inputs(I, Mappeds),
  spawn_link(Node, fun() -> Parent ! {self(), map_reduce:reduce_seq(Reduce,Inputs)} end).


%% Solution 2: Load-balancing Map-Reduce %%

map_reduce_balanced(Map,M,Reduce,R,Input) ->
  Workers  = spawn_workers(),
  io:format("Spawned workers: ~p~n", [Workers]),
  Splits   = map_reduce:split_into(M,Input),
  Mappers  = wrap_mappers(Map, R, Splits),
  Mappeds  = worker_pool(Mappers),
  Indices  = lists:seq(0,R-1),
  Reducers = wrap_reducers(Reduce, Mappeds, Indices),
  Reduceds = worker_pool(Reducers),
  kill_workers(Workers),
  flush(),
  lists:sort(lists:flatten(Reduceds)).


% Wrap mappers in anon funs for worker pool
wrap_mappers(Map, R, Splits) ->
  [wrap_map(Map, R, Split) || Split <- Splits].

% Wrap single map
wrap_map(Map, R, Split) ->
  fun() ->
    Mapped = get_mapped(Map, R, Split),
    map_reduce:group(lists:sort(Mapped))
  end.

% Wrap reducers in anon funs for worker pool
wrap_reducers(Reduce, Mappeds, Indices) ->
  [wrap_reducer(Reduce, I, Mappeds) || I <- Indices].
  
% Wrap single reduce  
wrap_reducer(Reduce, I, Mappeds) ->
  fun() ->
    Inputs = get_inputs(I, Mappeds),
    map_reduce:reduce_seq(Reduce,Inputs)
  end.

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
  spawn_link(Node, master, work, [Master]).

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
      io:format("Assigned task!~n"),
      Pid ! {task, F}
  end,
  Pid.

recieve_result(Pid) ->
  receive
    {result, Pid, Res} ->
      io:format("Received result!~n"), 
      Res
  end.

test_list() ->
  [fun() -> ok1 end, fun() -> ok2 end].

%% Helpers %%

% Intermediate code for mappers
get_mapped(Map, R, Split) ->
  [{erlang:phash2(K2,R),{K2,V2}} || {K,V} <- Split, {K2,V2} <- Map(K,V)]. 

% Intermediate code for reducers
get_inputs(I, Mappeds) ->
  [KV || Mapped <- Mappeds, {J,KVs} <- Mapped, I==J, KV <- KVs].

%% Benchmark one of the solutions
bench() ->
  {T, _} = timer:tc(page_rank, page_rank_balanced, []),
  %{T, V}.
  io:format("Time: ~pms.~n", [T/1000]).
  
%% Opens the dets file on all nodes
init_dets() ->
  [ rpc:call(Node, master, open_dets, []) || Node <- all_nodes()].

%% Get number of available schedulers from each node
get_schedulers() ->
  Self = {node(), erlang:system_info(schedulers)-1},
  Others = [ {Node, rpc:call(Node, erlang, system_info, [schedulers])} 
           || Node <- nodes()],
  [Self | Others].  

%% Opens the dets file
open_dets() ->
  dets:open_file(web,[{file,"web.dat"}]).

%% Return all available nodes
all_nodes() -> 
  [node() | nodes()].

%% Crawl given url to given depth and save results to file
crawl_and_save(Url, D) ->
  KVs = crawl:crawl(Url, D),
  dets:open_file("web.dat", []),
  dets:insert("web.dat", KVs),
  dets:close("web.dat").
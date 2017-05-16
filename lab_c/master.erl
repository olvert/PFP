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
  Workers  = worker:spawn_workers(),
  Splits   = map_reduce:split_into(M,Input),
  Mappers  = wrap_mappers(Map, R, Splits),
  Mappeds  = worker:worker_pool(Mappers),
  Indices  = lists:seq(0,R-1),
  Reducers = wrap_reducers(Reduce, Mappeds, Indices),
  Reduceds = worker:worker_pool(Reducers),
  worker:kill_workers(Workers),
  worker:flush(),
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

%% Solution 3: Fault-tolerant Map-Reduce %%

map_reduce_fault(Map,M,Reduce,R,Input) ->
  fault_worker:spawn_pool(),
  receive
    {pool, registered} -> ok
  end,
  Splits   = map_reduce:split_into(M,Input),  
  Mappers  = wrap_mappers(Map, R, Splits),
  Mappeds  = fault_worker:worker_pool(Mappers),
  Indices  = lists:seq(0,R-1),
  Reducers = wrap_reducers(Reduce, Mappeds, Indices),
  Reduceds = fault_worker:worker_pool(Reducers),
  global:send(pool, {stop}),
  receive
    {pool, stopped} -> ok
  end,
  global:unregister_name(master),
  lists:sort(lists:flatten(Reduceds)).


%% Helpers %%

% Intermediate code for mappers
get_mapped(Map, R, Split) ->
  [{erlang:phash2(K2,R),{K2,V2}} || {K,V} <- Split, {K2,V2} <- Map(K,V)]. 

% Intermediate code for reducers
get_inputs(I, Mappeds) ->
  [KV || Mapped <- Mappeds, {J,KVs} <- Mapped, I==J, KV <- KVs].

%% Benchmark one of the solutions
bench() ->
  {T, _} = timer:tc(page_rank, page_rank_fault, []),
  io:format("Time: ~pms.~n", [T/1000]).

%% Return all available nodes
all_nodes() -> 
  [node() | nodes()].

%% Crawl given url to given depth and save results to file
crawl_and_save(Url, D) ->
  KVs = crawl:crawl(Url, D),
  dets:open_file("web.dat", []),
  dets:insert("web.dat", KVs),
  dets:close("web.dat").
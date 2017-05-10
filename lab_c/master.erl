-module(master).
-compile(export_all).

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
		Mapped = [{erlang:phash2(K2,R),{K2,V2}}
			  || {K,V} <- Split,
			     {K2,V2} <- Map(K,V)],
		Parent ! {self(),map_reduce:group(lists:sort(Mapped))}
	end).

spawn_reducers(_, _, _, _, []) ->
  [];
spawn_reducers([N|Ns], Parent, Reduce, Mappeds, [I|Is]) ->
  R  = spawn_reducer(N, Parent, Reduce, I, Mappeds),
  Rs = spawn_reducers(Ns++[N], Parent, Reduce, Mappeds, Is),
  [R|Rs].

spawn_reducer(Node,Parent,Reduce,I,Mappeds) ->
  Inputs = [KV
      || Mapped <- Mappeds,
	 {J,KVs} <- Mapped,
	 I==J,
	 KV <- KVs],
  spawn_link(Node, fun() -> Parent ! {self(), map_reduce:reduce_seq(Reduce,Inputs)} end).


%% Helpers %%

%% Benchmark one of the solutions
bench() ->
  {T, _} = timer:tc(page_rank, page_rank, []),
  io:format("Time: ~pms.~n", [T/1000]).
  
%% Opens the dets file on all nodes

%% Return all available nodes
all_nodes() -> 
  [node() | nodes()].

%% Crawl given url to given depth and save results to file
crawl_and_save(Url, D) ->
  KVs = crawl:crawl(Url, D),
  dets:open_file("web.dat", []),
  dets:insert("web.dat", KVs),
  dets:close("web.dat").
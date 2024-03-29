%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This implements a page rank algorithm using map-reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(page_rank).
-compile(export_all).

%% Use map_reduce to count word occurrences

map(Url,ok) ->
    {ok,web} = dets:open_file(web,[{file,"web.dat"}]),
    [{Url,Body}] = dets:lookup(web,Url),
    Urls = crawl:find_urls(Url,Body),
    [{U,1} || U <- Urls].

reduce(Url,Ns) ->
    [{Url,lists:sum(Ns)}].

page_rank() ->
    Urls = load_data(),
    map_reduce:map_reduce_seq(fun map/2, fun reduce/2, 
			      [{Url,ok} || Url <- Urls]).

page_rank_par() ->
    Urls = load_data(),
    map_reduce:map_reduce_par(fun map/2, 32, fun reduce/2, 8, 
			      [{Url,ok} || Url <- Urls]).

page_rank_dist() ->
    Urls = load_data(),
    master:map_reduce_dist(fun map/2, 32, fun reduce/2, 8, 
			      [{Url,ok} || Url <- Urls]).
          
page_rank_balanced() ->
    Urls = load_data(),
    master:map_reduce_balanced(fun map/2, 32, fun reduce/2, 8, 
			      [{Url,ok} || Url <- Urls]).

page_rank_fault() ->
    Urls = load_data(),
    master:map_reduce_fault(fun map/2, 32, fun reduce/2, 8, 
			      [{Url,ok} || Url <- Urls]).
          
load_data() ->
  dets:open_file(web,[{file,"web.dat"}]),
  dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web).
  

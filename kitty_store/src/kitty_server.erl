-module(kitty_server).
% -compile(export_all).
-include("records.hrl").
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([handle_cast/2, handle_call/3, init/1]).

start_link() -> g_server:start_link(?MODULE, []).

init([]) -> [].

order_cat(Pid, Name, Color, Description) ->
  g_server:call(Pid, {order, Name, Color, Description}).

return_cat(Pid, Cat) ->
  g_server:cast(Pid, Cat).

close_shop(Pid) ->
  g_server:call(Pid, terminate).

handle_cast({return, ReturnedCat = #cat{}}, AllCats) ->
  [ReturnedCat|AllCats].

handle_call({order, Name, Color, Description}, From, AllCats) ->
  if AllCats =:= [] ->
      g_server:reply(From, make_cat(Name, Color, Description)),
      AllCats;
     AllCats =/= [] ->
      g_server:reply(From, hd(AllCats)),
      tl(AllCats)
  end;

handle_call(terminate, From, AllCats) ->
  g_server:reply(From, ok),
  terminate(AllCats).

make_cat(Name, Color, Description) ->
  #cat{name=Name, color=Color, description=Description}.

terminate(Cats) ->
  [io:format("~s was set free!~n", [C#cat.name]) || C <- Cats],
  exit(normal).
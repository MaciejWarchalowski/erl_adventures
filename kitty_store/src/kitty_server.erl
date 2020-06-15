-module(kitty_server).
-compile(export_all).
-include("records.hrl").

start_link() -> spawn_link(fun init/0).

init() -> loop([]).

order_cat(Pid, Name, Color, Description) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, {order, Name, Color, Description}},
  receive
    {Ref, Cat} ->
      erlang:demonitor(Ref,[flush]),
      Cat;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

return_cat(Pid, Cat) ->
  Pid ! {return, Cat},
  ok.

close_shop(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, terminate},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref),
      ok;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

loop(AllCats) ->
  receive
    {Pid, Ref, {order, Name, Color, Description}} ->
      if AllCats =:= [] ->
          Pid ! {Ref, make_cat(Name, Color, Description)},
          loop(AllCats);
         AllCats =/= [] ->
          Pid ! {Ref, hd(AllCats)},
          loop(tl(AllCats))
      end;
    {return, ReturnedCat = #cat{}} ->
      loop([ReturnedCat|AllCats]);
    {Pid, Ref, terminate} ->
      Pid ! {Ref, ok},
      terminate(AllCats);
    Unknown ->
      io:format("Unknown message received ~p~n", [Unknown]),
      loop(AllCats)
  end.

make_cat(Name, Color, Description) ->
  #cat{name=Name, color=Color, description=Description}.

terminate(Cats) ->
  [io:format("~s was set free!~n", [C#cat.name]) || C <- Cats],
  ok.
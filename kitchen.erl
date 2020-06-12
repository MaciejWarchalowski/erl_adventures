-module(kitchen).
-compile(export_all).

fridge1() ->
  receive
    {From, {store, _Food}} ->
      From ! {self(), ok},
      fridge1();
    {From, {take, _Food}} ->
      From ! {self(), not_found},
      fridge1();
    terminate ->
      ok
  end.

fridge2(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge2([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), ok},
          fridge2(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge2(FoodList)
      end;
    terminate ->
      ok
  end.

store(Pid, Food) ->
  command(Pid, Food, store).

take(Pid, Food) ->
  command(Pid, Food, take).

command(Pid, Food, Action) ->
  Pid ! {self(), {Action, Food}},
  receive
    {_From, Msg} -> Msg
  end.

start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).
-module(event).
-compile(export_all).
-record(state, {server,
                name="",
                to_go=0}).

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, Delay) ->
  loop(#state{server=Server,
              name=EventName,
              to_go=normalize(Delay)}).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid), % In case process is dead.
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

loop(S = #state{server = Server, to_go = [T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
    after T * 1000 ->
      if
        Next =:= [] ->
          Server ! {done, S#state.name};
        Next =/= [] ->
          loop(S#state{to_go=Next})
      end
  end.

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used.
normalize(N) ->
    Limit = 49*24*60*60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].
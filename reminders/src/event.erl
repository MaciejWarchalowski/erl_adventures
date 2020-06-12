-module(event).
-compile(export_all).
-record(state, {server,
                name="",
                to_go=0}).

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
-module(evserver).
-compile(export_all).

-record(state, {
                events, % List of #event{} server spawned.
                clients % List of clients subscribed to events.
               }).

-record(event,
{
  name,
  description,
  pid,
  timeout = {{1970,1,1},{0,0,0}}
}).

init() ->
  loop(#state{events = orddic:new(),
              clients = orddic:new()}).

loop(S = #state{clients = Clients, events = Events}) ->
  receive
    {From, Ref, {subscribe, Client}} ->
      ClientRef = erlang:monitor(process, Client),
      NewClients = orddic:store(ClientRef, Client, Clients),
      From ! {Ref, ok},
      loop(S#state{clients = NewClients});
    {From, Ref, {add, Name, Description, DateTime}} ->
      case valid_datetime(DateTime) of
        true ->
          NewEvent = event:start_link(Name, DateTime),
          NewEvents = orddic:store(Name, #event{
            name=Name,
            description=Description,
            timeout=DateTime,
            pid=NewEvent}),
          From ! {Ref, ok},
          loop(S#state{events=NewEvents});
        false ->
          From ! {Ref, {error, bad_timeout}},
          loop(S)
      end;
    {From, Ref, {cancel, Name}} ->
      NewEvents = case orddic:find(Name, Events) of
        {ok, E} ->
          event:cancel(E#event.pid),
          orrdic:erase(Name, Events);
        error ->
          Events
      end,
      From ! {Ref, ok},
      loop(S#state{events=NewEvents});
    {done, Name} ->
      case orddic:find(Name, Events) of
        {ok, E} ->
          ok = send_to_clients({done, E#event.name, E#event.description}, Clients),
          loop(S#state{events = orddic:erase(Name, Events)});
        {error} ->
          loop(S)
      end;
    shutdown ->
      ok;
    code_change ->
      ok;
    {'DOWN', Ref, process, _Reason} ->
      ok;
    Unknown ->
      io:format("Unknown message: ~p ~n", [Unknown]),
      loop(S)
  end.

send_to_clients(Msg, Clients) ->
  error.

valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch error:function_clause ->
    false
  end;

valid_datetime(_) -> false.

valid_time({H, M, S}) when H >= 0, H < 24,
                           M >= 0, M < 60,
                           S >= 0, S < 60 -> true;
valid_time(_) -> false.
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

loop(S = #state{clients = Clients, events = Events}) ->
  receive
    {From, Ref, {subscribe, Client}} ->
      ClientRef = erlang:monitor(process, Client),
      NewClients = orddict:store(ClientRef, Client, Clients),
      From ! {Ref, ok},
      loop(S#state{clients = NewClients});
    {From, Ref, {add, Name, Description, DateTime}} ->
      case valid_datetime(DateTime) of
        true ->
          NewEvent = event:start_link(Name, DateTime),
          NewEvents = orddict:store(Name, #event{
            name=Name,
            description=Description,
            timeout=DateTime,
            pid=NewEvent}, Events),
          From ! {Ref, ok},
          loop(S#state{events=NewEvents});
        false ->
          From ! {Ref, {error, bad_timeout}},
          loop(S)
      end;
    {From, Ref, {cancel, Name}} ->
      NewEvents = case orddict:find(Name, Events) of
        {ok, E} ->
          event:cancel(E#event.pid),
          orrdic:erase(Name, Events);
        error ->
          Events
      end,
      From ! {Ref, ok},
      loop(S#state{events=NewEvents});
    {done, Name} ->
      case orddict:find(Name, Events) of
        {ok, E} ->
          send_to_clients({done, E#event.name, E#event.description}, Clients),
          loop(S#state{events = orddict:erase(Name, Events)});
        {error} ->
          loop(S)
      end;
    shutdown ->
      exit(shutdown);
    code_change ->
      ?MODULE:loop(S);
    {'DOWN', Ref, process, _Pid, _Reason} ->
      loop(S#state{clients=orddict:erase(Ref, Clients)});
    Unknown ->
      io:format("Unknown message: ~p ~n", [Unknown]),
      loop(S)
  end.

send_to_clients(Msg, Clients) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, Clients).

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

start() ->
  register(?MODULE, Pid = spawn(?MODULE, init, [])),
  Pid.

start_link() ->
  register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
  Pid.

init() ->
  loop(#state{events = orddict:new(),
              clients = orddict:new()}).

terminate() ->
  ?MODULE ! shutdown.

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} -> {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} -> {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

add_event(Name, Description, DateTime) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, DateTime}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.

listen(Delay) ->
  receive
    E = {done, _Name, _Description} ->
      [E|listen(0)]
  after Delay * 1000 ->
    []
  end.
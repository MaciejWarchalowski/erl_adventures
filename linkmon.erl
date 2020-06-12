-module(linkmon).
-compile(export_all).

start_critic() ->
  spawn(?MODULE, critic, []).

judge(Artist, Album) ->
  critic ! {self(), {Artist, Album}},
  Pid = whereis(critic), % race condition possible here.
  receive
    {Pid, Verdict} -> Verdict
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
      From ! {self(), "They are great!"};
    {From, {"System of a Downtime", "Memoize"}} ->
      From ! {self(), "They're not Johnny Crash but they're good."};
    {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
      From ! {self(), "Simply incredible."};
    {From, {_Artist, _Album}} ->
      From ! {self(), "Horrible."}
  end,
  critic().

start_critic2() ->
  spawn(?MODULE, restarter, []).

restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic, []),
  register(critic, Pid),
  receive
    {'EXIT', Pid, normal} -> %not a crash
      io:format("Normal..~n"),
      ok;
    {'EXIT', Pid, shutdown} -> % normal termination
      io:format("Shutdown..~n"),
      ok;
    {'EXIT', Pid, _} ->
      io:format("Restarting...~n"),
      restarter()
  end.

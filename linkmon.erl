-module(linkmon).
-compile(export_all).

start_critic() ->
  spawn(?MODULE, critic, []).

judge(Artist, Album) ->
  Ref = make_ref(),
  critic ! {self(), Ref, {Artist, Album}},
  receive
    {Ref, Verdict} -> Verdict
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
      From ! {Ref, "They are great!"};
    {From, Ref, {"System of a Downtime", "Memoize"}} ->
      From ! {Ref, "They're not Johnny Crash but they're good."};
    {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
      From ! {Ref, "Simply incredible."};
    {From, Ref, {_Artist, _Album}} ->
      From ! {Ref, "Horrible."}
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

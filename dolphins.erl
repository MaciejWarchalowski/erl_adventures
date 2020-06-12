-module(dolphins).
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip ->
      io:format("Dolphin flippin~n");
    fish ->
      io:format("Yum Yum Yum~n");
    _ ->
      io:format("I got nuttin~n")
  end.

dolphin2() ->
  receive
    {From, do_a_flip} ->
      From ! "Dolphin flippin";
    {From, fish} ->
      From ! "Yum Yum Yum";
    _ ->
      io:format("I got nuttin~n")
  end.

dolphin3() ->
  receive
    {From, do_a_flip} ->
      From ! "Dolphin flippin",
      dolphin3();
    {From, fish} ->
      From ! "Yum Yum Yum, Bye!";
    _ ->
      io:format("I got nuttin~n"),
      dolphin3()
  end.
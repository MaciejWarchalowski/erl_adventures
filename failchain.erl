-module(failchain).
-compile(export_all).

chain(0) ->
  receive
    _ -> ok
  after 2000 ->
    exit("chain dies here")
  end;

chain(N) ->
  spawn_link(?MODULE, chain, [N-1]),
  receive
    _ -> ok
  end.
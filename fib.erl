-module(recursive).
-export([fact/1]).

fact(N) when N == 0 -> 1;
fact(N) when N > 0 -> N * fact(N - 1).

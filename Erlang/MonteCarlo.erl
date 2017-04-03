% Boniface Sindala
% Andrew Markley
% CS401
% Assignment 4

-module(montecarlo).
-export([start/2, rand/0, contains/2, spawnThreads/2, sum/3, count/2]).

start(Iterations, Actors) ->
  Chunk = Iterations div Actors,
  register(result, spawn(montecarlo, sum, [Iterations, Actors, 0])),
  spawnThreads( Actors, Chunk ).

spawnThreads( 0, Chunk ) ->
  spawn(montecarlo, count, [Chunk, 0] );
spawnThreads( Actor, Chunk ) ->
  spawn(montecarlo, count, [Chunk, 0]),
  spawnThreads( Actor-1, Chunk ).

sum(Iterations, Actors, Count) ->
  receive
    {Counter} ->
      if
        Actors - 1 > 0 ->
          sum(Iterations, Actors - 1, Count + Counter);
        true ->
          io:fwrite("Approximation to Pi: ~w~n", [( Count + Counter) / Iterations * 4 ])
      end
   end.

count( 0, Counter ) ->
  result ! {Counter};
count( Round, Counter ) ->
  C = contains( 1, rand() ),
  if
    C -> count( Round-1, Counter + 1 );
    true -> count( Round-1, Counter )
  end.

rand() ->
  <<I1:32/unsigned-integer,
  I2:32/unsigned-integer,
  I3:32/unsigned-integer>> = crypto:strong_rand_bytes(12),
  rand:seed(exsplus, {I1, I2, I3}),
  {rand:uniform(), rand:uniform()}.

contains( Radius, Coords ) ->
  X = element(1, Coords),
  Y = element(2, Coords),
  math:sqrt( X*X + Y*Y) < Radius.

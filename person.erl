-module(person).
-include("person.hrl").
-export([make_person/1, admin_access/1, make_admin/1]).

make_person(G) ->
  #person{
    first_name = "Maciek",
    last_name = "Gaciek",
    age = 100,
    group = G,
    title = "SE"
  }.

admin_access(#person{ first_name = Name, group = admin}) ->
  io:format("Access granted for ~s~n", [Name]);
admin_access(#person{}) ->
  io:format("Access Denied~n");
admin_access(R) ->
  erlang:error({non_person, R}).

make_admin(P = #person{group = Group, first_name = Name}) when Group =:= employee ->
  io:format("Changing group for ~s from ~s to admin~n", [Name, Group]),
  {changed, P#person{group = admin}}.
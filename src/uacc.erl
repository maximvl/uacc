-module(uacc).

-export([start/0, stop/0]).
-export([add_record/2, modify/3]).

-type data_type() :: list | plist | dict | array | tuple | record.

start() -> application:start(uacc).
stop() -> application:stop(uacc).

add_record(Name, Fields) ->
  uacc_storage:add_record(Name, Fields).

-spec data_type(any()) -> data_type().
data_type([{_, _}|_]) -> plist;
data_type(L) when is_list(L) -> list;
data_type(D) when is_tuple(D) andalso element(1, D) == dict -> dict;
data_type(D) when is_tuple(D) andalso element(1, D) == array -> array;
data_type(D) when is_tuple(D) andalso is_atom(element(1, D)) ->
  Name = element(1, D),
  case uacc_storage:get_fields(Name) of
    {ok, _} -> record;
    {error, not_found} -> tuple
  end;
data_type(D) when is_tuple(D) -> tuple;
data_type(X) -> throw({not_supported, X}).

position(E, L) -> position(E, L, 1).
position(_, [], _) -> not_found;
position(E, [E|_], N) -> N;
position(E, [_|T], N) -> position(E, T, N+1).

-spec modify(function(), list(), any()) -> any().
modify(F, [], S) -> F(S);
modify(F, [{T,K}|Path], S) when is_function(F, 1) ->
  modify(T, F, [K|Path], S);
modify(F, Path, S) when is_function(F, 1) andalso is_list(Path) ->
  modify(data_type(S), F, Path, S).

-spec modify(data_type(), function(), list(), any()) -> any().
modify(plist, F, [K|Rest], S) ->
  {P1, S2, P2} = plist_split(K, S),
  Val = modify(F, Rest, S2),
  P1 ++ [{K, Val}|P2];
modify(list, F, [N|Rest], S) when is_integer(N) ->
  {P1, S2, P2} = list_split(N, S),
  Val = modify(F, Rest, S2),
  P1 ++ [Val|P2];
modify(dict, F, [K|Rest], S) ->
  S2 = dict:fetch(K, S),
  Val = modify(F, Rest, S2),
  dict:store(K, Val, S);
modify(array, F, [N|Rest], S) when is_integer(N) ->
  S2 = array:get(N, S),
  Val = modify(F, Rest, S2),
  array:set(N, Val, S);
modify(tuple, F, [N|Rest], S) when is_integer(N) ->
  S2 = element(N, S),
  Val = modify(F, Rest, S2),
  setelement(N, S, Val);
modify(record, F, [K|Rest], S) when is_atom(K) ->
  {ok, Fields} = uacc_storage:get_fields(element(1, S)),
  case position(K, Fields) of
    not_found -> throw({not_found, K});
    N ->
      Val = modify(F, Rest, element(N+1, S)),
      setelement(N+1, S, Val)
  end.

list_split(N, L) -> list_split(N, L, []).
list_split(1, [H|T], Acc) -> {lists:reverse(Acc), H, T};
list_split(N, [H|T], Acc) -> list_split(N-1, T, [H|Acc]).

plist_split(K, L) -> plist_split(K, L, []).
plist_split(K, [{K, V}|T], Acc) -> {lists:reverse(Acc), V, T};
plist_split(K, [H|T], Acc) -> plist_split(K, T, [H|Acc]).

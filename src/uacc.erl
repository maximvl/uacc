-module(uacc).

-export([start/0, stop/0]).
-export([add_record/2, update/3, get/2, record_fields/1]).
-export([list_split/2, plist_split/2, position/2]).

-type data_type() :: list | plist | dict | array | tuple | record.

start() -> application:start(uacc).
stop() -> application:stop(uacc).

-spec add_record(atom(), [atom()]) -> ok.
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

-spec update(function(), list(), any()) -> any().
update(F, [], S) -> F(S);
update(F, [{T,K}|Path], S) when is_function(F, 1) ->
  update(T, F, [K|Path], S);
update(F, Path, S) when is_function(F, 1) andalso is_list(Path) ->
  update(data_type(S), F, Path, S).

-spec update(data_type(), function(), list(), any()) -> any().
update(plist, F, [K|Rest], S) ->
  {P1, S2, P2} = plist_split(K, S),
  Val = update(F, Rest, S2),
  P1 ++ [{K, Val}|P2];
update(list, F, [N|Rest], S) when is_integer(N) ->
  {P1, S2, P2} = list_split(N, S),
  Val = update(F, Rest, S2),
  P1 ++ [Val|P2];
update(dict, F, [K|Rest], S) ->
  S2 = dict:fetch(K, S),
  Val = update(F, Rest, S2),
  dict:store(K, Val, S);
update(array, F, [N|Rest], S) when is_integer(N) ->
  S2 = array:get(N, S),
  Val = update(F, Rest, S2),
  array:set(N, Val, S);
update(tuple, F, [N|Rest], S) when is_integer(N) ->
  S2 = element(N, S),
  Val = update(F, Rest, S2),
  setelement(N, S, Val);
update(record, F, [K|Rest], S) when is_atom(K) ->
  {ok, Fields} = uacc_storage:get_fields(element(1, S)),
  case position(K, Fields) of
    not_found -> throw({not_found, K});
    N ->
      Val = update(F, Rest, element(N+1, S)),
      setelement(N+1, S, Val)
  end.

-spec list_split(integer(), list()) -> {list(), any(), list()}.
list_split(N, L) -> list_split(N, L, []).
list_split(_, [], _) -> throw(out_of_range);
list_split(1, [H|T], Acc) -> {lists:reverse(Acc), H, T};
list_split(N, [H|T], Acc) -> list_split(N-1, T, [H|Acc]).

-spec plist_split(any(), list()) -> {list(), any(), list()}.
plist_split(K, L) -> plist_split(K, L, []).
plist_split(_, [], _) -> throw(not_found);
plist_split(K, [{K, V}|T], Acc) -> {lists:reverse(Acc), V, T};
plist_split(K, [H|T], Acc) -> plist_split(K, T, [H|Acc]).

-spec position(any(), list()) -> not_found | integer().
position(E, L) -> position(E, L, 1).
position(_, [], _) -> not_found;
position(E, [E|_], N) -> N;
position(E, [_|T], N) -> position(E, T, N+1).

-spec get(list(), any()) -> any().
get([], S) -> S;
get([{T,K}|Rest], S) -> get(T, [K|Rest], S);
get(Path, S) -> get(data_type(S), Path, S).

-spec get(data_type(), list(), any()) -> any().
get(plist, [K|Rest], S) ->
    S2 = proplists:get_value(K, S),
    get(Rest, S2);
get(list, [N|Rest], S) when is_integer(N) ->
    S2 = lists:nth(N, S),
    get(Rest, S2);
get(dict, [K|Rest], S) ->
    S2 = dict:fetch(K, S),
    get(Rest, S2);
get(array, [N|Rest], S) when is_integer(N) ->
    S2 = array:get(N, S),
    get(Rest, S2);
get(tuple, [N|Rest], S) when is_integer(N) ->
    S2 = element(N, S),
    get(Rest, S2);
get(record, [K|Rest], S) when is_atom(K) ->
    {ok, Fields} = uacc_storage:get_fields(element(1, S)),
    case position(K, Fields) of
        not_found -> throw(not_found);
        N ->
            S2 = element(N+1, S),
            get(Rest, S2)
    end.

record_fields(R) ->
  {ok, F} = uacc_storage:get_fields(R),
  F.

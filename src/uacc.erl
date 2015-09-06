-module(uacc).

-export([start/0, stop/0]).
-export([get/2, update/3]).
-export([add_record/2, record_fields/1, make_record/1, make_record/2,
         record_to_plist/1]).

-type data_type() :: uacc_util:data_type().
-type record() :: tuple().

start() -> application:start(uacc).
stop() -> application:stop(uacc).

-spec add_record(atom(), [atom()]) -> ok.
add_record(Name, Fields) when is_atom(Name) ->
  L = length(Fields)+1,
  Enumerated = lists:zip(lists:seq(1, L), ['$record'|Fields]),
  uacc_storage:add_record(Name, Enumerated).

-spec update(function() | any(), list(), any()) -> any().
update(F, [], S) when is_function(F, 1) -> F(S);
update(F, [], _) -> F;
update(F, [{T,K}|Path], S) ->
  update(T, F, [K|Path], S);
update(F, Path, S) when is_list(Path) ->
  update(uacc_util:data_type(S), F, Path, S).

-spec update(data_type(), function(), list(), any()) -> any().
update(plist, F, [K|Rest], S) ->
  case uacc_util:plist_split(K, S) of
    {P1, S2, P2} ->
      Val = update(F, Rest, S2),
      P1 ++ [{K, Val}|P2];
    not_found -> throw({not_found, K})
  end;
update(list, F, [N|Rest], S) when is_integer(N) ->
  case uacc_util:list_split(N, S) of
    {P1, S2, P2} ->
      Val = update(F, Rest, S2),
      P1 ++ [Val|P2];
    out_of_range -> throw({out_of_range, N})
  end;
update(dict, F, [K|Rest], S) ->
  S2 = dict:fetch(K, S),
  Val = update(F, Rest, S2),
  dict:store(K, Val, S);
update(array, F, [N|Rest], S) when is_integer(N) ->
  S2 = array:get(N, S),
  Val = update(F, Rest, S2),
  array:set(N, Val, S);
%% record
update(tuple, F, [K|Rest], S)
  when is_atom(K) andalso is_atom(element(1, S)) ->
  RecName = element(1, S),
  case uacc_storage:get_fields(RecName) of
    {ok, Fields} ->
      case lists:keyfind(K, 2, Fields) of
        {N, _} ->
          Val = update(F, Rest, element(N, S)),
          setelement(N, S, Val);
        false -> throw({not_found, K})
      end;
    {error, not_found} -> throw({not_found, RecName})
  end;
%% real tuple
update(tuple, F, [N|Rest], S) when is_integer(N) ->
  case tuple_size(S) < N of
    true -> throw({out_of_range, N});
    false ->
      S2 = element(N, S),
      Val = update(F, Rest, S2),
      setelement(N, S, Val)
  end.

-spec get(list(), any()) -> any().
get([], S) -> S;
get([{T,K}|Rest], S) -> get(T, [K|Rest], S);
get(Path, S) -> get(uacc_util:data_type(S), Path, S).

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
%% record
get(tuple, [K|Rest], S) when is_atom(K) andalso is_atom(element(1, S)) ->
  RecName = element(1, S),
  case uacc_storage:get_fields(RecName) of
    {ok, Fields} ->
      case lists:keyfind(K, 2, Fields) of
        {N, _} ->
          S2 = element(N, S),
          get(Rest, S2);
        false -> throw({not_found, K})
      end;
    {error, not_found} -> throw({not_found, RecName})
  end;
get(tuple, [N|Rest], S) when is_integer(N) ->
  S2 = element(N, S),
  get(Rest, S2).

-spec record_fields(atom()) -> [{integer(), atom()}].
record_fields(R) when is_atom(R) ->
  case uacc_storage:get_fields(R) of
    {ok, Fields} -> Fields;
    {error, not_found} -> throw(not_found)
  end.

-spec make_record(atom()) -> record().
make_record(N) -> make_record(N, []).

-spec make_record(atom(), [{atom(), any()}]) -> record().
make_record(RecName, Defaults) ->
  case uacc_storage:get_fields(RecName) of
    {ok, Fields} ->
      RDefaults = [case lists:keyfind(K, 2, Fields) of
                     {N, _} -> {N, V};
                     false -> throw({not_found, K})
                   end || {K, V} <- Defaults],
      L = length(Fields),
      erlang:make_tuple(L, [], [{1, RecName} | RDefaults]);
    {error, not_found} -> throw({not_found, RecName})
  end.

-spec record_to_plist(record()) -> [{atom(), any()}].
record_to_plist(R) when is_atom(element(1, R)) ->
  N = element(1, R),
  case uacc_storage:get_fields(N) of
    {ok, Fields} ->
      Tlist = erlang:tuple_to_list(R),
      lists:zipwith(fun({_, F}, V) -> {F, V} end, Fields, Tlist);
    {error, not_found} -> throw(not_found)
  end.

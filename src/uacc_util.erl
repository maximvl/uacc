-module(uacc_util).
-compile(export_all).

-type data_type() :: list | plist | dict | array | tuple | record.

-spec data_type(any()) -> data_type().
data_type([{_, _}|_]) -> plist;
data_type(L) when is_list(L) -> list;
data_type(D) when is_tuple(D) andalso element(1, D) == dict -> dict;
data_type(D) when is_tuple(D) andalso element(1, D) == array -> array;
%% Record is determined later, using access keys
data_type(D) when is_tuple(D) -> tuple;
data_type(X) -> throw({not_supported, X}).

-spec list_split(integer(), list()) -> {list(), any(), list()} | out_of_range.
list_split(N, L) -> list_split(N, L, []).
list_split(_, [], _) -> out_of_range;
list_split(1, [H|T], Acc) -> {lists:reverse(Acc), H, T};
list_split(N, [H|T], Acc) -> list_split(N-1, T, [H|Acc]).

-spec plist_split(any(), list()) -> {list(), any(), list()} | not_found.
plist_split(K, L) -> plist_split(K, L, []).
plist_split(_, [], _) -> not_found;
plist_split(K, [{K, V}|T], Acc) -> {lists:reverse(Acc), V, T};
plist_split(K, [H|T], Acc) -> plist_split(K, T, [H|Acc]).

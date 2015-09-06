-module(utils_SUITE).
-compile(export_all).

-record(x, {}).

init_per_suite(Config) ->
  ok = application:ensure_started(uacc),
  Config.

end_per_suite(_Config) ->
  application:stop(uacc).

all() ->
  [test_utils, test_datatypes].

test_utils(_Config) ->
  L = [a, b, c, d, e, f],
  {[], a, [b, c, d, e, f]} = uacc_util:list_split(1, L),
  {[a, b], c, [d, e, f]} = uacc_util:list_split(3, L),
  out_of_range = uacc_util:list_split(10, L),

  Pl = [{a, 1}, {b, 2}, {c, 3}],
  {[], 1, [{b, 2}, {c, 3}]} = uacc_util:plist_split(a, Pl),
  {[{a,1}],2,[{c,3}]} =  uacc_util:plist_split(b, Pl),
  {[{a,1},{b,2}],3,[]} = uacc_util:plist_split(c, Pl),
  not_found = uacc_util:plist_split(1, Pl).

test_datatypes(_Config) ->
  list = uacc_util:data_type([]),
  plist = uacc_util:data_type([{a, 1}]),
  dict = uacc_util:data_type(dict:new()),
  array = uacc_util:data_type(array:new()),
  tuple = uacc_util:data_type({}),
  tuple = uacc_util:data_type(#x{}),
  ct_utils:throws(fun() -> uacc_util:data_type(5) end,
                  throw, {not_supported, 5}).

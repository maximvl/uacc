-module(utils_SUITE).
-compile(export_all).

init_per_suite(Config) ->
  ok = application:ensure_started(uacc),
  Config.

end_per_suite(_Config) ->
  application:stop(uacc).

all() ->
  [test_utils].

test_utils(_Config) ->
  L = [a, b, c, d, e, f],
  {[], a, [b, c, d, e, f]} = uacc:list_split(1, L),
  {[a, b], c, [d, e, f]} = uacc:list_split(3, L),
  ok = throws(fun() -> uacc:list_split(10, L) end,
              throw, out_of_range),

  Pl = [{a, 1}, {b, 2}, {c, 3}],
  {[], 1, [{b, 2}, {c, 3}]} = uacc:plist_split(a, Pl),
  {[{a,1}],2,[{c,3}]} =  uacc:plist_split(b, Pl),
  {[{a,1},{b,2}],3,[]} = uacc:plist_split(c, Pl),

  ok = throws(fun() -> uacc:plist_split(1, Pl) end,
              throw, not_found),

  1 = uacc:position(a, [a, b, c]),
  2 = uacc:position(b, [a, b, c]),
  3 = uacc:position(c, [a, b, c]),
  not_found = uacc:position(z, [a, b, c]).

throws(F, Type, Desc) ->
  try F()
  catch Type:Desc ->
      ok
  end.

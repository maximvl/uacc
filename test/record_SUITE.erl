-module(record_SUITE).
-compile(export_all).

-record(x, {x1, x2, x3}).

init_per_suite(Config) ->
  ok = application:ensure_started(uacc),
  uacc:add_record(x, record_info(fields, x)),
  Config.

end_per_suite(_Config) ->
  application:stop(uacc).

all() ->
  [test_get, test_update, test_records].

test_get(_Config) ->
  X = #x{x1 = 5, x2 = {a, b, c}, x3 = #x{x2=test}},
  5 = uacc:get([x1], X),
  c = uacc:get([x2, 3], X),
  test = uacc:get([x3, x2], X),
  undefined = uacc:get([x3, x1], X).

test_update(_Config) ->
  X = #x{x1 = 5, x2 = [b], x3 = #x{x2=test}},
  true = X#x{x1 = 6} == uacc:update(fun(_) -> 6 end, [x1], X),
  true = X#x{x2 = [a,b]} == uacc:update(fun(E) -> [a|E] end, [x2], X),
  true = X#x{x2 = [a]} == uacc:update(fun(_) -> a end, [x2, 1], X),
  true = X#x{x3 = #x{x2=fail}} == uacc:update(fun(_) -> fail end, [x3, x2], X).

test_records(_Config) ->
  [x1, x2, x3] = uacc:record_fields(x),
  uacc:add_record(y, record_info(fields, x)),
  [x1, x2, x3] = uacc:record_fields(y).

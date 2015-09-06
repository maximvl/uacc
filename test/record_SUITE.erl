-module(record_SUITE).
-compile(export_all).

-record(x, {x1, x2, x3}).
-record(y, {y1}).

init_per_suite(Config) ->
  ok = application:ensure_started(uacc),
  uacc:add_record(x, record_info(fields, x)),
  Config.

end_per_suite(_Config) ->
  application:stop(uacc).

all() ->
  [test_get, test_update, test_records, test_errors].

test_get(_Config) ->
  X = #x{x1 = 5, x2 = {a, b, c}, x3 = #x{x2=test}},
  5 = uacc:get([x1], X),
  c = uacc:get([x2, 3], X),
  test = uacc:get([x3, x2], X),
  undefined = uacc:get([x3, x1], X).

test_update(_Config) ->
  X = #x{x1 = 5, x2 = [b], x3 = #x{x2=test}},
  true = X#x{x1 = 6} == uacc:update(6, [x1], X),
  true = X#x{x2 = [a,b]} == uacc:update(fun(E) -> [a|E] end, [x2], X),
  true = X#x{x2 = [a]} == uacc:update(a, [x2, 1], X),
  true = X#x{x3 = #x{x2=fail}} == uacc:update(fun(_) -> fail end, [x3, x2], X).

test_records(_Config) ->
  [{1, '$record'}, {#x.x1, x1},
   {#x.x2, x2}, {#x.x3, x3}] = uacc:record_fields(x),
  X = #x{x1 = a, x2 = b, x3 = c},
  true = X == uacc:make_record(x, [{x2, b}, {x3, c}, {x1, a}]),
  true = [{'$record', x}, {x1, X#x.x1},
          {x2, X#x.x2}, {x3, X#x.x3}] == uacc:record_to_plist(X).

test_errors(_Config) ->
  ct_utils:throws(fun() -> uacc:get([field_dosnt_exist], #x{}) end,
                  throw, {not_found, field_dosnt_exist}),
  ct_utils:throws(fun() -> uacc:update(z, [field_dosnt_exist], #x{}) end,
                  throw, {not_found, field_dosnt_exist}),
  ct_utils:throws(fun() -> uacc:update(6, [x1], #y{}) end,
                  throw, {not_found, y}),
  ct_utils:throws(fun() -> uacc:get([y1], #y{}) end,
                  throw, {not_found, y}).

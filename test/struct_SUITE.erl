-module(struct_SUITE).
-compile(export_all).

init_per_suite(Config) ->
  ok = application:ensure_started(uacc),
  Config.

end_per_suite(_Config) ->
  application:stop(uacc).

all() ->
  [test_get, test_update].

test_get(_Config) ->
  X = [[{b, c}, {a, {0, 1, 2}}, {c, 7}], []],
  [{b,c},{a,{0,1,2}},{c,7}] =  uacc:get([1], X),
  {0,1,2} = uacc:get([1, a], X),
  0 = uacc:get([1, a, 1], X).

test_update(_Config) ->
  X = [{a, [{x, 0}, {v, 10}, {y, 15}], x}, b, c, d],
  X2 = uacc:update(fun(_) -> new end, [1], X),
  [new, b, c, d] = X2,
  X3 = uacc:update(fun(_) -> new end, [1, 2], X),
  [{a,new,x},b,c,d] = X3,
  X4 = uacc:update(fun(_) -> new end, [1, 2, v], X),
  [{a,[{x,0},{v,new},{y,15}],x},b,c,d] = X4,
  X5 = uacc:update(fun(V) -> V+1 end, [1, 2, v], X),
  [{a,[{x,0},{v,11},{y,15}],x},b,c,d] = X5.

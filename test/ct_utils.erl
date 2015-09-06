-module(ct_utils).
-compile([export_all]).

throws(F, Type, Desc) ->
  try F()
  catch Type:Desc ->
      ok
  end.

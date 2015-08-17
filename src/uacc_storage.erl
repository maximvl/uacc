%%%-------------------------------------------------------------------
%%% @author  <maxvel>
%%% @copyright (C) 2015,
%%% @doc
%%%
%%% @end
%%% Created : 17 Aug 2015 by  <maxvel>
%%%-------------------------------------------------------------------
-module(uacc_storage).

-behaviour(gen_server).

%% API
-export([start_link/0, get_fields/1, add_record/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_record(atom(), [atom()]) -> ok.
add_record(Name, Fields) when is_atom(Name) andalso is_list(Fields) ->
  gen_server:call(?SERVER, {add, Name, Fields}).

-spec get_fields(atom()) -> {ok, [atom()]} | {error, not_found}.
get_fields(Name) when is_atom(Name) ->
  case ets:lookup(?TAB, Name) of
    [{_, Fields}] -> {ok, Fields};
    [] -> {error, not_found}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?TAB, [named_table]),
  {ok, #state{}}.

handle_call({add, Name, Fields}, _From, State) ->
  ets:insert(?TAB, {Name, Fields}),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

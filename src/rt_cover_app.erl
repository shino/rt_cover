-module(rt_cover_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([is_debug/0,
         data_dir/0,
         result_dir/0]).

-include("rt_cover.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Apps = [list_to_atom(App) ||
           App <-string:tokens(os:getenv("RT_COVER_APPS"), ",")],
  rt_cover:cover_compile(data_dir(), Apps),
  rt_cover_sup:start_link().

stop(_State) ->
  try
    rt_cover:export_data(data_dir()),
    ok
  catch
    Class:Reason ->
      ?debugVal({Class, Reason, erlang:get_stacktrace()}),
      exit(Reason)
  end.
    
is_debug() ->
  %% TODO(shino): application env
  true.

data_dir() ->
  filename:absname(code:lib_dir(rt_cover, coverdata)).

result_dir() ->
  %% TODO(shino): Look os environment variable
  filename:absname(code:lib_dir(rt_cover, result)).

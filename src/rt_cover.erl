-module(rt_cover).

-export([start/0]).
-export([cover_compile/2,
         export_data/1,
         analyze/1]).

-include("rt_cover.hrl").

start() ->
  application:start(rt_cover).

cover_compile(_DataDir, []) ->
  ok;
cover_compile(DataDir, [App | Rest]) ->
  cover:start(),
  cover:reset(),
  EBinDir = code:lib_dir(App, ebin),
  BeamFiles = filelib:wildcard("*.beam", EBinDir),
  lists:foreach(fun(Beam) ->
                  {ok, Mod} = cover:compile_beam(filename:join(EBinDir, Beam)),
                  ?debugLog({cover_compiled, Mod}),
                  DataFile = filename:join(DataDir, Mod),
                  case filelib:is_file(DataFile) of
                    false ->
                      ok;
                    true ->
                      ?debugLog({import, DataFile}),
                      cover:import(DataFile)
                  end
                end,
                BeamFiles),
  cover_compile(DataDir, Rest).
  
export_data(OutDir) ->
  ?debugLog({export_data, OutDir}),
  ok = filelib:ensure_dir(OutDir),
  os:cmd("rm -rf " ++ OutDir),
  ok = file:make_dir(OutDir),
  export_data(OutDir, cover:modules()).

export_data(_OutDir, []) ->
  ok;
export_data(OutDir, [Mod | Rest]) ->
  OutFile = filename:join([OutDir, atom_to_list(Mod)]),
  ok = cover:export(OutFile, Mod),
  ?debugVal({export_data, OutFile}),
  export_data(OutDir, Rest).

analyze(ResultDir) ->
  ?debugLog({analyze, ResultDir}),
  ok = filelib:ensure_dir(ResultDir),
  os:cmd("rm -rf " ++ ResultDir),
  ok = file:make_dir(ResultDir),
  analyze(ResultDir, cover:modules()).

analyze(_ResultDir, []) ->
  %% TODO(shino): Generate coverage index.html (cf: rebar_eunit)
  ok;
analyze(ResultDir, [Mod | Rest]) ->
  ?debugLog({analyze, Mod}),
  OutFile = filename:join([ResultDir, atom_to_list(Mod) ++ ".html"]),
  cover:analyze_to_file(Mod, OutFile, [html]),
  analyze(ResultDir, Rest).

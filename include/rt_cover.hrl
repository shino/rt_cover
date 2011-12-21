-include_lib("eunit/include/eunit.hrl").

-define(debugLog(E), 
        case rt_cover_app:is_debug() of
          true ->
            ?debugVal(E);
          false ->
            ok
        end).

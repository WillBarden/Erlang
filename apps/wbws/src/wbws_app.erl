-module(wbws_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
            '_',
            [
                {"/", index_h, []}
            ]
        }
    ]),
    case string:uppercase(os:getenv("PROD", "FALSE")) of
        "TRUE" -> noop;
        _ -> cowboy:start_clear(wbws, [{port, 80}], #{
            env => #{dispatch => Dispatch}
        })
    end,
    wbws_sup:start_link().

stop(_State) ->
    ok.

-module(wbws_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:fwrite("Starting server...~n", []),
    Dispatch = cowboy_router:compile([
        {
            '_',
            [
                { "/", index_h, [] },
                { "/register", register_h, [] },
                { "/auth", auth_h, [] },
                { "/tables/[:id]", tables_h, [] },
                { "/tables/:id/:ws", tables_h, [] },
                { "/[...]", cowboy_static, { priv_dir, wbws, "/static" } }
            ]
        }
    ]),
    Port = list_to_integer(string:trim(os:getenv("PORT", "80"))),
    cowboy:start_clear(wbws, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    wbws_sup:start_link().

stop(_State) ->
    ok.

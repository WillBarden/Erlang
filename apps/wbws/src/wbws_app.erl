-module(wbws_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
            '_',
            [
                % { "/login", login_h, { unprotected, [] } },
                % { "/logout", logout_h, { unprotected, [] } },
                { "/", index_h, { unprotected, [] } },
                { "/register", register_h, { unprotected, [] } },
                { "/auth", auth_h, { unprotected, [] } },
                { "/[...]", cowboy_static, { unprotected, { priv_dir, wbws, "/static" } } }
            ]
        }
    ]),
    Port = list_to_integer(string:trim(os:getenv("PORT", "80"))),
    cowboy:start_clear(wbws, [{port, Port}], #{
        env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, auth_m, cowboy_handler]
    }),
    wbws_sup:start_link().

stop(_State) ->
    ok.

-module(wbws_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
            '_',
            [
                {"/", index_h, []},
                {"/register", register_h, []},
                {"/login", login_h, []},
                {"/home", cowboy_static, {priv_file, wbws, "home.html"}},
                {"/[...]", cowboy_static, {priv_dir, wbws, "/static"}}
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

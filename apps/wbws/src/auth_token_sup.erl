-module(auth_token_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

init([]) ->
    HMACKey  = unicode:characters_to_binary(os:getenv("HMAC_KEY")),
    WorkerArgs = #{ hmac_key => HMACKey },

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [
        poolboy:child_spec(
            auth_token_pool,
            [{ name, { local, auth_token_pool } }, { worker_module, auth_token }, { size, 8 }, { max_overflow, 32 }],
            WorkerArgs
        )
    ],
    { ok, { SupFlags, ChildSpecs } }.

start_link() -> supervisor:start_link(?MODULE, []).

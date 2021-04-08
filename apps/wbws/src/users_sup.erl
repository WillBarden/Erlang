-module(users_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

init([]) ->
    SecSalt = unicode:characters_to_binary(os:getenv("SEC_SALT")),
    WorkerArgs = #{ sec_salt => SecSalt },
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [
        poolboy:child_spec(
            users_worker_pool,
            [{ name, { local, users_worker_pool } }, { worker_module, users }, { size, 4 }, { max_overflow, 8 }],
            WorkerArgs
        )
    ],
    { ok, { SupFlags, ChildSpecs } }.

start_link() -> supervisor:start_link(?MODULE, []).

-module(db_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [
        poolboy:child_spec(
            db_worker_pool,
            [{ name, { local, db_worker_pool } }, { worker_module, db }, { size, 8 }, { max_overflow, 32 }]
        )
    ],
    { ok, { SupFlags, ChildSpecs } }.

start_link() -> supervisor:start_link(?MODULE, []).

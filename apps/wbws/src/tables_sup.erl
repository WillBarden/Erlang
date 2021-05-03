-module(tables_sup).
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
            tables_pool,
            [{ name, { local, tables_pool } }, { worker_module, tables }, { size, 8 }, { max_overflow, 32 }],
            []
        )
    ],
    { ok, { SupFlags, ChildSpecs } }.

start_link() -> supervisor:start_link(?MODULE, []).

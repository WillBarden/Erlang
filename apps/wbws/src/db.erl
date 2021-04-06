-module(db).
-behaviour(supervisor).

-export([init/1, start_link/0]).
-export([connect/0, equery/2, equery/3, squery/1, squery/2, transact/1]).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [
        poolboy:child_spec(
            ?MODULE, [{ name, { local, ?MODULE } }, { worker_module, db_w }, { size, 8 }, { max_overflow, 64 }]
        )
    ],
    { ok, { SupFlags, ChildSpecs } }.

start_link() -> supervisor:start_link(?MODULE, []).

connect() -> 
    Host = string:trim(os:getenv("DB_HOST", "localhost")),
    Port = list_to_integer(string:trim(os:getenv("DB_PORT", "5432"))),
    UseSSL = case string:trim(os:getenv("DB_SSL", "true")) of
        "false" -> false;
        _ -> required 
    end,
    Username = string:trim(os:getenv("DB_USERNAME", "username")),
    Password = string:trim(os:getenv("DB_PASSWORD", "password")),
    Database = string:trim(os:getenv("DB_DATABASE", "database")),
    Timeout = list_to_integer(string:trim(os:getenv("DB_TIMEOUT", "3000"))),
    case epgsql:connect(#{
        host => Host, port => Port, ssl => UseSSL, username => Username, password => Password, database => Database,
        timeout => Timeout
    }) of
        {ok, Conn} -> Conn;
        Other -> Other
    end.


equery(Conn, Query, Params) -> epgsql:equery(Conn, Query, Params).

equery(Query, Params) ->
    poolboy:transaction(
        ?MODULE,
        fun(Worker) -> gen_server:call(Worker, { equery, Query, Params }) end
    ).

squery(Conn, Query) -> epgsql:squery(Conn, Query).

squery(Query) ->
    poolboy:transaction(
        ?MODULE, 
        fun(Worker) -> gen_server:call(Worker, { squery, Query }) end
    ).

transact(Fn) ->
    poolboy:transaction(
        ?MODULE,
        fun(Worker) -> gen_server:call(Worker, { transaction, Fn }) end
    ).

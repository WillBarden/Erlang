-module(db).
-behaviour(gen_server).

-export([init/1, terminate/2, start_link/1, handle_call/3, handle_cast/2]).
-export([connect/0, equery/2, equery/3, squery/1, squery/2, transact/1]).

init(_Args) ->
    Conn = case connect() of { error, _Error } -> null; { ok, C } -> C end,
    { ok, #{ db_conn => Conn } }.

terminate(_Reason, #{ db_conn := Conn } = _State) ->
    epgsql:close(Conn),
    normal.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

handle_call({ squery, Query }, _From, #{ db_conn := Conn } = _State) ->
    { reply, epgsql:squery(Conn, Query), _State };
handle_call({ squery, Conn, Query }, _From, _State) ->
    { reply, epgsql:squery(Conn, Query), _State };
handle_call({ equery, Query, Params }, _From, #{ db_conn := Conn } = _State) ->
    { reply, epgsql:equery(Conn, Query, Params), _State };
handle_call({ equery, Conn, Query, Params }, _From, _State) ->
    { reply, epgsql:equery(Conn, Query, Params), _State };
handle_call({ transaction, Fn }, _From, #{ db_conn := Conn } = _State) ->
    { reply, epgsql:with_transaction(Conn, Fn, []), _State };
handle_call({ transaction, Conn, Fn }, _From, _State) ->
    { reply, epgsql:with_transaction(Conn, Fn, []), _State };
handle_call(_Request, _From, _State) -> { reply, _Request, _State }.

handle_cast(_Request, _State) -> { noreply, _State }.

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
    epgsql:connect(#{
        host => Host, port => Port, ssl => UseSSL, username => Username, password => Password, database => Database,
        timeout => Timeout
    }).

call(Request) -> poolboy:transaction(db_worker_pool, fun(Worker) -> gen_server:call(Worker, Request) end).

cast(Request) -> poolboy:transaction(db_worker_pool, fun(Worker) -> gen_server:cast(Worker, Request) end).

squery(Conn, Query) -> call({ squery, Conn, Query }).

squery(Query) -> call({ squery, Query }).

equery(Conn, Query, Params) -> call({ equery, Conn, Query, Params }).

equery(Query, Params) -> call({ equery, Query, Params }).

transact(Conn, Fn) -> call({ transaction, Conn, Fn }).

transact(Fn) -> call({ transaction, Fn }).

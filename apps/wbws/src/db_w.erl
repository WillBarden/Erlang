-module(db_w).
-behaviour(gen_server).

-export([init/1, start_link/1, handle_call/3, handle_cast/2]).
-export([connect/0]).

init(_Args) ->
    C = case connect() of { error, _Error } -> null; { ok, Conn } -> Conn end,
    { ok, #{ db_conn => C } }.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

handle_call({ equery, Query, Params }, _From, #{ db_conn := C } = _State) ->
    { reply, epgsql:equery(C, Query, Params), _State };
handle_call({ squery, Query }, _From, #{ db_conn := C } = _State) ->
    { reply, epgsql:squery(C, Query), _State };
handle_call({ transaction, Fn }, _From, #{ db_conn := C } = _State) ->
    { transaction, epgsql:with_transaction(C, Fn, []), _State };
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
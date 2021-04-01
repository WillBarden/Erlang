-module(db).

-export([connect/0]).

connect() -> 
    Host = string:trim(os:getenv("DB_HOST", "localhost")),
    Port = list_to_integer(string:trim(os:getenv("DB_PORT", "5432"))),
    Username = string:trim(os:getenv("DB_USERNAME", "username")),
    Password = string:trim(os:getenv("DB_PASSWORD", "password")),
    Database = string:trim(os:getenv("DB_DATABASE", "database")),
    Timeout = list_to_integer(string:trim(os:getenv("DB_TIMEOUT", "3000"))),
    case epgsql:connect(#{
        host => Host, port => Port, username => Username, password => Password, database => Database, timeout => Timeout
    }) of
        {ok, Conn} -> Conn;
        Other -> Other
    end.
